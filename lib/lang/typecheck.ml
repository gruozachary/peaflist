open! Base
open Result
open Let_syntax

module Make_var () : sig
  type t

  val zero : t
  val succ : t -> t

  include Comparable.S with type t := t
end = struct
  module T = struct
    type t = int [@@deriving compare, sexp_of]

    let zero = 0
    let succ x = x + 1
  end

  include T
  include Comparable.Make (T)
end

module Gen_var = Make_var ()
module Uni_var = Make_var ()

type ty =
  | TUni of tu ref
  | TGen of Gen_var.t
  | TFun of ty * ty
  | TProd of ty list
  | TCon of Type_ident.t * ty list

and tu =
  | Unbound of Uni_var.t
  | Link of ty

type scheme = Forall of Gen_var.t list * ty

let rec prune = function
  | TUni ({ contents = Link ty } as tu_ref) ->
    let root = prune ty in
    tu_ref := Link root;
    root
  | ty -> ty
;;

let rec unify ty1 ty2 =
  let unify_lists tys tys' =
    let%bind ty_pairs =
      match List.zip tys tys' with
      | List.Or_unequal_lengths.Ok ty_pairs -> return ty_pairs
      | List.Or_unequal_lengths.Unequal_lengths -> fail "Type unification failed"
    in
    let%map _ = List.map ~f:(fun (ty, ty') -> unify ty ty') ty_pairs |> Result.all in
    ()
  in
  match prune ty1, prune ty2 with
  | TUni tu_ref, TUni tu_ref' when phys_equal tu_ref tu_ref' -> return ()
  | TUni tu_ref, ty | ty, TUni tu_ref -> return (tu_ref := Link ty)
  | TFun (ty_fun, ty_arg), TFun (ty_fun', ty_arg') ->
    let%bind _ = unify ty_fun ty_fun' in
    unify ty_arg ty_arg'
  | TProd tys, TProd tys' -> unify_lists tys tys'
  | TCon (ident, tys), TCon (ident', tys') when Type_ident.equal ident ident' ->
    unify_lists tys tys'
  | TGen gen_var, TGen gen_var' when Gen_var.equal gen_var gen_var' -> return ()
  | _ -> fail "Type unification failed"
;;

type tenv_entry =
  { arity : int
  ; ctors : Constr_ident.t list
  }

type cenv_entry =
  { parent : Type_ident.t
  ; tag : int
  ; arg_scheme_opt : scheme option
  ; res_schme : scheme
  }

type ctx =
  { mutable next_gen_var : Gen_var.t
  ; mutable next_uni_var : Uni_var.t
  ; env : (Var_ident.t, scheme, Var_ident.comparator_witness) Map.t
  ; tenv : (Type_ident.t, tenv_entry, Type_ident.comparator_witness) Map.t
  ; cenv : (Constr_ident.t, cenv_entry, Type_ident.comparator_witness) Map.t
  }

let rec free_in_ty ty =
  match ty with
  | TUni { contents = Unbound uni_var } -> Set.singleton (module Uni_var) uni_var
  | TUni { contents = Link _ } -> free_in_ty (prune ty)
  | TGen _ -> Set.empty (module Uni_var)
  | TFun (ty_fun, ty_arg) -> Set.union (free_in_ty ty_fun) (free_in_ty ty_arg)
  | TProd tys -> List.map tys ~f:free_in_ty |> Set.union_list (module Uni_var)
  | TCon (_, tys) -> List.map tys ~f:free_in_ty |> Set.union_list (module Uni_var)
  | _ -> .
;;

let free_in_env =
  Map.fold
    ~init:(Set.empty (module Uni_var))
    ~f:(fun ~key:_ ~data:(Forall (_, ty)) acc -> Set.union acc (free_in_ty ty))
;;

let fresh_gen_var ctx =
  let x = ctx.next_gen_var in
  ctx.next_gen_var <- Gen_var.succ x;
  x
;;

let fresh_uni_var ctx =
  let x = ctx.next_uni_var in
  ctx.next_uni_var <- Uni_var.succ x;
  x
;;

let create_tu_ref uni_var = TUni (Ref.create (Unbound uni_var))

let instantiate ctx (Forall (gen_vars, ty)) =
  let map =
    List.map ~f:(fun gen_var -> gen_var, fresh_uni_var ctx) gen_vars
    |> Map.of_alist_exn (module Gen_var)
  in
  let rec replace = function
    | TUni _ as ty -> ty
    | TGen gen_var as ty ->
      Map.find map gen_var |> Option.map ~f:create_tu_ref |> Option.value ~default:ty
    | TFun (ty_fun, ty_arg) -> TFun (replace ty_fun, replace ty_arg)
    | TProd tys -> TProd (List.map ~f:replace tys)
    | TCon (ident, tys) -> TCon (ident, List.map ~f:replace tys)
  in
  replace ty
;;

let generalise ctx ty =
  let free_uni_vars = Set.diff (free_in_ty ty) (free_in_env ctx.env) in
  let map =
    Set.fold
      ~init:(Map.empty (module Uni_var))
      ~f:(fun acc uni_var -> Map.set acc ~key:uni_var ~data:(fresh_gen_var ctx))
      free_uni_vars
  in
  let rec replace = function
    | TUni { contents = Unbound uni_var } as ty ->
      Map.find map uni_var
      |> Option.map ~f:(fun gen_var -> TGen gen_var)
      |> Option.value ~default:ty
    | TUni { contents = Link _ } as ty -> replace (prune ty)
    | TGen _ as ty -> ty
    | TFun (ty_fun, ty_arg) -> TFun (replace ty_fun, replace ty_arg)
    | TProd tys -> TProd (List.map ~f:replace tys)
    | TCon (ident, tys) -> TCon (ident, List.map ~f:replace tys)
  in
  replace ty
;;
