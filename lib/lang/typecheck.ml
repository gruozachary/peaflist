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
  ; gen_vars : Gen_var.t list
  ; arg_gens : ty list
  ; res_gen : ty
  }

type ctx =
  { mutable next_gen_var : Gen_var.t
  ; mutable next_uni_var : Uni_var.t
  ; env : (Var_ident.t, scheme, Var_ident.comparator_witness) Map.t
  ; tenv : (Type_ident.t, tenv_entry, Type_ident.comparator_witness) Map.t
  ; cenv : (Constr_ident.t, cenv_entry, Type_ident.comparator_witness) Map.t
  }

let term_fetch ctx ident =
  match Map.find ctx.env ident with
  | Some ty -> ty
  | None -> raise_s [%message "Internal compiler error: Variable out of scope"]
;;

let type_fetch ctx ident =
  match Map.find ctx.tenv ident with
  | Some entry -> entry
  | None -> raise_s [%message "Internal compiler error: Type out of scope"]
;;

let ctor_fetch ctx ident =
  match Map.find ctx.cenv ident with
  | Some entry -> entry
  | None -> raise_s [%message "Internal compiler error: Constructor out of scope"]
;;

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

let nullary_type_of_str (rename : Rename.t) str =
  match Rename.Renamer.fetch ~str rename.type_renamer with
  | Some ident -> TCon (ident, [])
  | None -> raise_s [%message "Internal compiler error: Unknown type"]
;;

let fresh_gen_var ctx =
  let x = ctx.next_gen_var in
  ctx.next_gen_var <- Gen_var.succ x;
  x
;;

let scheme_of_ty ty = Forall ([], ty)

let fresh_tu_ref ctx =
  let x = ctx.next_uni_var in
  ctx.next_uni_var <- Uni_var.succ x;
  TUni (Ref.create (Unbound x))
;;

let instantiate_multi ctx gen_vars tys =
  let map =
    List.map ~f:(fun gen_var -> gen_var, fresh_tu_ref ctx) gen_vars
    |> Map.of_alist_exn (module Gen_var)
  in
  let rec replace = function
    | TUni _ as ty -> ty
    | TGen gen_var as ty -> Map.find map gen_var |> Option.value ~default:ty
    | TFun (ty_fun, ty_arg) -> TFun (replace ty_fun, replace ty_arg)
    | TProd tys -> TProd (List.map ~f:replace tys)
    | TCon (ident, tys) -> TCon (ident, List.map ~f:replace tys)
  in
  List.map ~f:replace tys
;;

let instantiate ctx (Forall (gen_vars, ty)) = instantiate_multi ctx gen_vars [ ty ]

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

include Ast.Make (struct
    type var_ident = Var_ident.t
    type ctor_ident = Constr_ident.t
    type type_ident = Type_ident.t
    type tvar = int

    module Ext = struct
      module Pat = struct
        type for_int = unit
        type for_ident = ty
        type for_tuple = ty
        type for_constr = ty
      end

      module Expr = struct
        type for_int = unit
        type for_ident = ty
        type for_constr = ty
        type for_apply = ty
        type for_group = Ast.void
        type for_lambda = ty
        type for_let = scheme
        type for_match = ty
        type for_tuple = ty
        type for_binop = Ast.void
      end

      module Ty = struct
        type for_var = ty
        type for_con = ty
        type for_prod = ty
        type for_fun = ty
      end

      module Decl = struct
        type for_val = unit
        type for_type = unit
      end

      module Prog = struct
        type for_prog = unit
      end
    end
  end)

module Pat = struct
  include Pat

  let ty_of (rename : Rename.t) (pat : t) =
    match pat with
    | Int _ -> nullary_type_of_str rename "int"
    | Ident (_, ty) -> ty
    | Tuple (_, ty) -> ty
    | Constr (_, _, ty) -> ty
  ;;

  let rec infer (ctx : ctx) (rename : Rename.t) (pat : Desugared_ast.Pat.t) =
    let module O = Desugared_ast.Pat in
    let safe_merge map map' =
      let open Option.Let_syntax in
      Map.fold map' ~init:(Some map) ~f:(fun ~key ~data acc ->
        let%bind map_acc = acc in
        match Map.add map_acc ~key ~data with
        | `Ok map_new -> Some map_new
        | `Duplicate -> None)
    in
    let infer_list pats =
      let%map pats, env =
        List.fold
          ~init:(return ([], Map.empty (module Var_ident)))
          ~f:(fun acc pat ->
            let%bind pats_acc, env_acc = acc in
            let%bind pat_inf, env_inf = infer ctx rename pat in
            let%map env_merged =
              safe_merge env_acc env_inf
              |> of_option ~error:"Redefinition of variable in pattern match"
            in
            pat_inf :: pats_acc, env_merged)
          pats
      in
      List.rev pats, env
    in
    match pat with
    | O.Int (x, ()) -> return (Int (x, ()), Map.empty (module Var_ident))
    | O.Ident (ident, ()) ->
      let ty = fresh_tu_ref ctx in
      return (Ident (ident, ty), Map.singleton (module Var_ident) ident ty)
    | O.Tuple (pats, ()) ->
      let%map pats, env = infer_list pats in
      Tuple (pats, TProd (List.map ~f:(ty_of rename) pats)), env
    | O.Constr (ident, pats, ()) ->
      let entry = ctor_fetch ctx ident in
      let arg_tys, res_ty =
        match instantiate_multi ctx entry.gen_vars (entry.res_gen :: entry.arg_gens) with
        | res_ty :: arg_tys -> arg_tys, res_ty
        | _ -> assert false
      in
      let%bind pats_inf, env_inf = infer_list pats in
      let%bind zipped =
        match List.zip arg_tys (List.map ~f:(ty_of rename) pats_inf) with
        | List.Or_unequal_lengths.Ok zipped -> return zipped
        | List.Or_unequal_lengths.Unequal_lengths -> fail "Arity for constructor wrong"
      in
      let%map () =
        List.fold zipped ~init:(return ()) ~f:(fun acc (ty, ty') ->
          let%bind () = acc in
          unify ty ty')
      in
      Constr (ident, pats_inf, res_ty), env_inf
  ;;
end
