open! Base
open Ast

type alpha = string

module rec Tau : sig
  type t =
    | TVar of alpha
    | TFun of t * t
    | TProd of t list
    | TCon of alpha * t list

  val to_string : t -> string
  val free_tvars : t -> (alpha, String.comparator_witness) Set.t
  val occurs : alpha -> t -> bool
  val of_ty : vm:(alpha, alpha, String.comparator_witness) Map.t -> Ty.t -> t
  val apply_sub : t -> sub:Subst.t -> t
end = struct
  type t =
    | TVar of alpha
    | TFun of t * t
    | TProd of t list
    | TCon of alpha * t list

  let prec = function
    | TVar _ -> 3
    | TFun _ -> 1
    | TProd _ -> 2
    | TCon _ -> 3
  ;;

  let rec to_string ty =
    let go ty' =
      if prec ty' < prec ty then "(" ^ to_string ty' ^ ")" else to_string ty'
    in
    match ty with
    | TVar x -> x
    | TFun (t, t') -> go t ^ " -> " ^ go t'
    | TProd ts ->
      List.map ~f:go ts
      |> List.intersperse ~sep:" * "
      |> List.fold ~init:"" ~f:String.append
    | TCon (x, []) -> x
    | TCon (x, [ t ]) -> go t ^ " " ^ x
    | TCon (x, ts) ->
      "("
      ^ (List.map ~f:go ts
         |> List.intersperse ~sep:" "
         |> List.fold ~init:"" ~f:String.append)
      ^ ") "
      ^ x
  ;;

  let rec free_tvars = function
    | TVar tvar -> Set.of_list (module String) [ tvar ]
    | TFun (t0, t1) -> Set.union (free_tvars t0) (free_tvars t1)
    | TProd ts -> Set.union_list (module String) (List.map ~f:(fun t -> free_tvars t) ts)
    | TCon (_, ts) ->
      Set.union_list (module String) (List.map ~f:(fun t -> free_tvars t) ts)
  ;;

  let rec occurs tvar t : bool =
    match t with
    | TVar x -> equal_string x tvar
    | TFun (t'0, t'1) -> occurs tvar t'0 || occurs tvar t'1
    | TProd ts -> List.exists ~f:(occurs tvar) ts
    | TCon (tvar', ts) -> equal_string tvar tvar' || List.exists ~f:(occurs tvar) ts
  ;;

  let rec of_ty ~vm = function
    | Ty.Id x -> TVar (Option.value (Map.find vm x) ~default:x)
    | Ty.Fun (t, t') -> TFun (of_ty ~vm t, of_ty ~vm t')
    | Ty.Prod ts -> TProd (List.map ~f:(of_ty ~vm) ts)
    | Ty.App (x, ts) -> TCon (x, List.map ~f:(of_ty ~vm) ts)
  ;;

  let rec apply_sub ty ~sub =
    match ty with
    | Tau.TVar x ->
      (match Map.find sub x with
       | Some ty' -> ty'
       | None -> ty)
    | Tau.TFun (ty0, ty1) -> TFun (apply_sub ~sub ty0, apply_sub ~sub ty1)
    | Tau.TProd tys -> TProd (List.map tys ~f:(apply_sub ~sub))
    | Tau.TCon (tvar, tys) -> TCon (tvar, List.map tys ~f:(apply_sub ~sub))
  ;;
end

(*TODO: consider changing this to set*)
and Scheme : sig
  type t = Forall of alpha list * Tau.t

  val to_string : t -> string
  val free_tvars : t -> (alpha, String.comparator_witness) Set.t
  val of_tau : Tau.t -> t
  val apply_sub : t -> sub:Subst.t -> t
end = struct
  type t = Forall of alpha list * Tau.t

  let to_string (Forall (qs, t)) =
    (List.intersperse ~sep:" " qs |> List.fold ~init:"forall " ~f:String.append)
    ^ " . "
    ^ Tau.to_string t
  ;;

  let free_tvars = function
    | Forall (qs, ty) -> Set.diff (Set.of_list (module String) qs) (Tau.free_tvars ty)
  ;;

  let of_tau ty = Forall ([], ty)

  let apply_sub sc ~sub =
    let (Scheme.Forall (qs, ty)) = sc in
    let sub =
      Map.filter_keys sub ~f:(fun tv -> List.exists qs ~f:(equal_string tv) |> not)
    in
    Scheme.Forall (qs, Tau.apply_sub ~sub ty)
  ;;
end

and Gamma : sig
  type t

  module Merge_element : sig
    include module type of Map.Merge_element

    type nonrec t = (Scheme.t, Scheme.t) t
  end

  val empty : unit -> t
  val introduce : t -> id:alpha -> sc:Scheme.t -> t
  val lookup : t -> id:alpha -> Scheme.t Option.t
  val map : t -> f:(Scheme.t -> Scheme.t) -> t
  val free_tvars : t -> (alpha, String.comparator_witness) Set.t
  val merge : t -> t -> f:(key:alpha -> Merge_element.t -> Scheme.t Option.t) -> t
  val apply_sub : t -> sub:Subst.t -> t
end = struct
  type t = (alpha, Scheme.t, String.comparator_witness) Map.t

  module Merge_element = struct
    include Map.Merge_element

    type nonrec t = (Scheme.t, Scheme.t) t
  end

  let empty () = Map.empty (module String)
  let introduce env ~id ~sc = Map.set env ~key:id ~data:sc
  let lookup env ~id = Map.find env id
  let map env ~f = Map.map ~f env

  let free_tvars env =
    Map.fold
      env
      ~init:(Set.empty (module String))
      ~f:(fun ~key:_ ~data acc -> Set.union acc (Scheme.free_tvars data))
  ;;

  let merge = Map.merge
  let apply_sub env ~sub = Gamma.map env ~f:(fun sc -> Scheme.apply_sub ~sub sc)
end

and Subst : sig
  type t = (alpha, Tau.t, String.comparator_witness) Map.t

  val empty : t
  val add : t -> tv:alpha -> ty:Tau.t -> (t, string) Result.t
  val compose : t -> t -> t
  val unify : Tau.t -> Tau.t -> (t, string) Result.t
end = struct
  type t = (alpha, Tau.t, String.comparator_witness) Map.t

  let empty : t = Map.empty (module String)

  let rec apply_type sub ~ty =
    match ty with
    | Tau.TVar x ->
      (match Map.find sub x with
       | Some ty' -> ty'
       | None -> ty)
    | Tau.TFun (ty0, ty1) -> TFun (apply_type sub ~ty:ty0, apply_type sub ~ty:ty1)
    | Tau.TProd tys -> TProd (List.map tys ~f:(fun ty -> apply_type sub ~ty))
    | Tau.TCon (tvar, tys) -> TCon (tvar, List.map tys ~f:(fun ty -> apply_type sub ~ty))
  ;;

  let add sub ~tv ~ty =
    match ty with
    | Tau.TVar tv' ->
      if equal_string tv tv'
      then Result.Ok sub
      else Result.Ok (Map.set sub ~key:tv ~data:ty)
    | _ ->
      if Tau.occurs tv ty
      then Result.Error "Recursive type variable definiton"
      else Result.Ok (Map.set sub ~key:tv ~data:ty)
  ;;

  let compose sub sub' =
    Map.merge sub sub' ~f:(fun ~key:_ e ->
      match e with
      | `Left ty -> Option.Some ty
      | `Right ty -> Option.Some ty
      | `Both (_, ty) -> Option.Some ty)
  ;;

  let rec unify ty0 ty1 =
    let open Result.Let_syntax in
    match ty0, ty1 with
    | Tau.TVar tv, ty | ty, Tau.TVar tv -> add empty ~tv ~ty
    | Tau.TFun (ty_l0, ty_r0), TFun (ty_l1, ty_r1) ->
      let%bind sub = unify ty_l0 ty_l1 in
      let%map sub' = unify (apply_type sub ~ty:ty_r0) (apply_type sub ~ty:ty_r1) in
      compose sub sub'
    | Tau.TProd tys, Tau.TProd tys' ->
      (match
         List.fold2
           ~init:(Result.Ok empty)
           ~f:(fun res_sub ty'0 ty'1 ->
             let%bind sub = res_sub in
             let%map sub' = unify (apply_type sub ~ty:ty'0) (apply_type sub ~ty:ty'1) in
             compose sub sub')
           tys
           tys'
       with
       | List.Or_unequal_lengths.Ok x -> x
       | List.Or_unequal_lengths.Unequal_lengths ->
         Result.Error "Tuple arity must be the same")
    (*TODO: abstract*)
    | Tau.TCon (x, tys), Tau.TCon (x', tys') when String.equal x x' ->
      unify (Tau.TProd tys) (Tau.TProd tys')
    | _ -> Result.Error "Type unification failed"
  ;;
end

module TyEnv : sig
  type t
  type arity = int

  val empty : t
  val introduce : t -> id:alpha -> arity:arity -> t
  val lookup : t -> id:alpha -> arity Option.t
end = struct
  type arity = int
  type t = (alpha, arity, String.comparator_witness) Map.t

  let empty : t = Map.empty (module String)
  let introduce env ~id ~arity = Map.set env ~key:id ~data:arity
  let lookup env ~id = Map.find env id
end

module State = struct
  type t = { mutable next : int }

  let create () = { next = 0 }

  let fresh_tv s =
    let v = s.next in
    s.next <- v + 1;
    "_'_tvar" ^ Int.to_string v
  ;;

  let fresh s = Tau.TVar (fresh_tv s)
end

type error = string

module Ctx : sig
  type t

  val empty : unit -> t

  module Env : sig
    val get : t -> Gamma.t
    val map : t -> f:(Gamma.t -> Gamma.t) -> t
  end

  module Tenv : sig
    val get : t -> TyEnv.t
    val map : t -> f:(TyEnv.t -> TyEnv.t) -> t
  end

  module State : sig
    val get : t -> State.t
    val map : t -> f:(State.t -> State.t) -> t
  end
end = struct
  type t =
    { env : Gamma.t
    ; tenv : TyEnv.t
    ; state : State.t
    }

  let empty () = { env = Gamma.empty (); state = State.create (); tenv = TyEnv.empty }

  module Env = struct
    let get ctx = ctx.env
    let map ctx ~f = { ctx with env = f ctx.env }
  end

  module Tenv = struct
    let get ctx = ctx.tenv
    let map ctx ~f = { ctx with tenv = f ctx.tenv }
  end

  module State = struct
    let get ctx = ctx.state
    let map ctx ~f = { ctx with state = f ctx.state }
  end
end

module W = struct
  let instantiate ctx = function
    | Scheme.Forall (qs, ty) ->
      let sub =
        List.fold
          ~init:(Map.empty (module String))
          ~f:(fun acc key -> Map.set acc ~key ~data:(Ctx.State.get ctx |> State.fresh_tv))
          qs
      in
      let rec replace sub ty =
        match ty with
        | Tau.TVar tv ->
          (match Map.find sub tv with
           | Some tv' -> Tau.TVar tv'
           | None -> ty)
        | Tau.TFun (ty0, ty1) -> TFun (replace sub ty0, replace sub ty1)
        | Tau.TProd tys -> TProd (List.map tys ~f:(replace sub))
        | Tau.TCon (tvar, tys) -> TCon (tvar, List.map tys ~f:(replace sub))
      in
      replace sub ty
  ;;

  let generalise ctx ty =
    let env_tvs = Ctx.Env.get ctx |> Gamma.free_tvars in
    let ty_tvs = Tau.free_tvars ty in
    Scheme.Forall (Set.diff ty_tvs env_tvs |> Set.to_list, ty)
  ;;

  let rec expr ctx e =
    let open Result.Let_syntax in
    match e with
    | Expr.Id x ->
      (match Ctx.Env.get ctx |> Gamma.lookup ~id:x with
       | Option.Some sc ->
         let ty = instantiate ctx sc in
         Result.Ok (Subst.empty, ty)
       | Option.None -> Result.Error "Unbound variable")
    | Expr.Constr x ->
      (match Ctx.Env.get ctx |> Gamma.lookup ~id:x with
       | Option.Some sc ->
         let ty = instantiate ctx sc in
         Result.Ok (Subst.empty, ty)
       | Option.None -> Result.Error "Unbound constructor")
    | Expr.Lambda (x, e) ->
      let ty = Ctx.State.get ctx |> State.fresh in
      let ctx = Ctx.Env.map ctx ~f:(Gamma.introduce ~id:x ~sc:(Scheme.of_tau ty)) in
      let%map s, ty' = expr ctx e in
      s, Tau.TFun (Tau.apply_sub ~sub:s ty, Tau.apply_sub ~sub:s ty')
    | Expr.Apply (ef, e) ->
      let%bind s, tyf = expr ctx ef in
      let ctx = Ctx.Env.map ctx ~f:(Gamma.apply_sub ~sub:s) in
      let%bind s', ty = expr ctx e in
      let tyv = Ctx.State.get ctx |> State.fresh in
      let%map s'' = Subst.unify (Tau.apply_sub ~sub:s' tyf) (Tau.TFun (ty, tyv)) in
      Subst.compose (Subst.compose s s') s'', Tau.apply_sub ~sub:s'' tyv
    | Expr.Binding (x, e, e') ->
      let%bind s, ty = expr ctx e in
      let ctx = Ctx.Env.map ctx ~f:(Gamma.apply_sub ~sub:s) in
      let sc = generalise ctx ty in
      let%map s', ty' = expr (Ctx.Env.map ctx ~f:(Gamma.introduce ~id:x ~sc)) e' in
      Subst.compose s s', ty'
    | Expr.Group e -> expr ctx e
    | Expr.Int _ -> Result.Ok (Subst.empty, Tau.TCon ("int", []))
    | Expr.BinOp (el, o, er) ->
      let open Expr.Bin_op in
      (match o with
       | Append -> Result.Error "TODO: remove"
       | Div | Mul | Plus | Sub ->
         let%bind s, ty = expr ctx el in
         let ctx = Ctx.Env.map ctx ~f:(Gamma.apply_sub ~sub:s) in
         let%bind s1, ty' = expr ctx er in
         let%bind s2 = Subst.unify (Tau.apply_sub ~sub:s1 ty) (Tau.TCon ("int", [])) in
         let%map s3 = Subst.unify (Tau.apply_sub ~sub:s2 ty') (Tau.TCon ("int", [])) in
         let s' = Subst.compose (Subst.compose (Subst.compose s s1) s2) s3 in
         s', Tau.TCon ("int", []))
    | Expr.Match (_, _) -> Result.Error "Match typechecking not implemented"
    | Expr.Tuple es ->
      let%map s, tys =
        List.fold
          ~init:(Result.Ok (Subst.empty, []))
          ~f:(fun acc e ->
            let%bind s, tys = acc in
            let ctx = Ctx.Env.map ctx ~f:(Gamma.apply_sub ~sub:s) in
            let%map s', ty = expr ctx e in
            Subst.compose s s', ty :: tys)
          es
      in
      let tys = List.map ~f:(Tau.apply_sub ~sub:s) tys in
      s, Tau.TProd (List.rev tys)
  ;;
end

let typecheck =
  let open Result.Let_syntax in
  let rec go ctx = function
    | ValDecl (x, e) :: ds ->
      let%bind s, ty = W.expr ctx e in
      let ctx = Ctx.Env.map ctx ~f:(Gamma.apply_sub ~sub:s) in
      let sc = W.generalise ctx ty in
      let ctx = Ctx.Env.map ctx ~f:(Gamma.introduce ~id:x ~sc) in
      go ctx ds
    | TypeDecl (x, utvs, ctors) :: ds ->
      let arity = List.length utvs in
      let get_tvs () =
        match
          Map.of_alist
            (module String)
            (List.map ~f:(fun tv -> tv, Ctx.State.get ctx |> State.fresh_tv) utvs)
        with
        | `Ok x -> Result.Ok x
        | `Duplicate_key _ -> Result.Error "Duplicate type variable"
      in
      let ctx = Ctx.Tenv.map ctx ~f:(TyEnv.introduce ~id:x ~arity) in
      let%bind ctx'' =
        List.fold
          ~init:(Result.Ok ctx)
          ~f:(fun ctx_opt (y, t_opt) ->
            let%bind ctx = ctx_opt in
            let%map tv_map = get_tvs () in
            let tvs = Map.data tv_map in
            let tyvs = List.map ~f:(fun tv -> Tau.TVar tv) tvs in
            Ctx.Env.map
              ctx
              ~f:
                (Gamma.introduce
                   ~id:y
                   ~sc:
                     (match t_opt with
                      | Option.Some t ->
                        Scheme.Forall
                          (tvs, Tau.TFun (Tau.of_ty ~vm:tv_map t, Tau.TCon (x, tyvs)))
                      | Option.None -> Scheme.Forall (tvs, Tau.TCon (x, tyvs)))))
          ctors
      in
      go ctx'' ds
    | [] -> Result.Ok ctx
  in
  go (Ctx.empty ())
;;
