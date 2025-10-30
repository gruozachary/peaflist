open! Base

type alpha = string

module Tau = struct
  type concrete =
    | Int
    | Unit
  [@@deriving eq]

  type t =
    | TVar of alpha
    | TFun of t * t
    | TProd of t list
    | TApp of alpha * t list
    | TCon of concrete

  let rec free_tvars = function
    | TVar tvar -> Set.of_list (module String) [ tvar ]
    | TFun (t0, t1) -> Set.union (free_tvars t0) (free_tvars t1)
    | TProd ts -> Set.union_list (module String) (List.map ~f:(fun t -> free_tvars t) ts)
    | TApp (_, ts) ->
      Set.union_list (module String) (List.map ~f:(fun t -> free_tvars t) ts)
    | TCon _ -> Set.empty (module String)
  ;;

  let rec occurs tvar t : bool =
    match t with
    | TVar x -> equal_string x tvar
    | TFun (t'0, t'1) -> occurs tvar t'0 || occurs tvar t'1
    | TProd ts -> List.exists ~f:(occurs tvar) ts
    | TApp (tvar', ts) -> equal_string tvar tvar' || List.exists ~f:(occurs tvar) ts
    | TCon _ -> false
  ;;
end

(*TODO: consider changing this to set*)
module Scheme = struct
  type t = Forall of alpha list * Tau.t

  let free_tvars = function
    | Forall (qs, ty) -> Set.diff (Set.of_list (module String) qs) (Tau.free_tvars ty)
  ;;
end

module Gamma : sig
  type t

  val empty : t
  val introduce : t -> alpha -> Scheme.t -> t
  val lookup : t -> alpha -> Scheme.t Option.t
  val map : t -> f:(Scheme.t -> Scheme.t) -> t
  val free_tvars : t -> (alpha, String.comparator_witness) Set.t
end = struct
  type t = (alpha, Scheme.t, String.comparator_witness) Map.t

  let empty : t = Map.empty (module String)
  let introduce env tid sc = Map.set env ~key:tid ~data:sc
  let lookup env tid = Map.find env tid
  let map env ~f = Map.map ~f env

  let free_tvars env =
    Map.fold
      env
      ~init:(Set.empty (module String))
      ~f:(fun ~key:_ ~data acc -> Set.union acc (Scheme.free_tvars data))
  ;;
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

module Subst = struct
  type t = (alpha, Tau.t, String.comparator_witness) Map.t

  let empty : t = Map.empty (module String)

  let rec apply_type ~sub ty =
    match ty with
    | Tau.TVar x ->
      (match Map.find sub x with
       | Some ty' -> ty'
       | None -> ty)
    | Tau.TFun (ty0, ty1) -> TFun (apply_type ~sub ty0, apply_type ~sub ty1)
    | Tau.TProd tys -> TProd (List.map tys ~f:(apply_type ~sub))
    | Tau.TApp (tvar, tys) -> TApp (tvar, List.map tys ~f:(apply_type ~sub))
    | Tau.TCon _ -> ty

  and apply_scheme ~sub sc =
    let (Scheme.Forall (qs, ty)) = sc in
    let sub' = Map.filter_keys sub ~f:(fun tv -> List.exists qs ~f:(equal_string tv)) in
    Scheme.Forall (qs, apply_type ~sub:sub' ty)

  and apply_gamma ~sub env = Gamma.map env ~f:(apply_scheme ~sub)

  let add ~sub tv ty =
    match ty with
    | Tau.TVar tv' ->
      if equal_string tv tv'
      then Result.Ok sub
      else Result.Ok (Map.add_exn sub ~key:tv ~data:ty)
    | _ ->
      if Tau.occurs tv ty
      then Result.Error "Recursive type variable definiton"
      else Result.Ok (Map.add_exn sub ~key:tv ~data:ty)
  ;;

  let rec unify ~sub ty0 ty1 =
    let open Result.Let_syntax in
    let ty0' = apply_type ~sub ty0 in
    let ty1' = apply_type ~sub ty1 in
    match ty0', ty1' with
    | TVar tv, ty | ty, TVar tv -> add ~sub tv ty
    | TCon c0, TCon c1 when Tau.equal_concrete c0 c1 -> Result.Ok sub
    | TFun (ty_l0, ty_r0), TFun (ty_l1, ty_r1) ->
      let%bind sub' = unify ~sub ty_l0 ty_l1 in
      unify ~sub:sub' (apply_type ~sub:sub' ty_r0) (apply_type ~sub:sub' ty_r1)
    | TProd tys, TProd tys' ->
      (match
         List.fold2
           ~init:(Result.Ok sub)
           ~f:(fun res_sub t'0 t'1 ->
             let%bind sub = res_sub in
             unify ~sub t'0 t'1)
           tys
           tys'
       with
       | List.Or_unequal_lengths.Ok x -> x
       | List.Or_unequal_lengths.Unequal_lengths ->
         Result.Error "Tuple arity must be the same")
    | TApp (_tv, _tys), TApp (_tv', _tys') -> assert false (* TODO: implement *)
    | _ -> assert false
  ;;

  let compose sub sub' =
    Map.merge sub sub' ~f:(fun ~key:_ e ->
      match e with
      | `Left ty -> Option.Some ty
      | `Right ty -> Option.Some ty
      | `Both (_, ty) -> Option.Some ty)
  ;;
end

type error = string

type ctx =
  { env : Gamma.t
  ; state : State.t
  ; subst : Subst.t
  }

module W = struct
  let instantiate ctx = function
    | Scheme.Forall (qs, ty) ->
      let sub =
        List.fold
          ~init:(Map.empty (module String))
          ~f:(fun acc key -> Map.set acc ~key ~data:(State.fresh_tv ctx.state))
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
        | Tau.TApp (tvar, tys) -> TApp (tvar, List.map tys ~f:(replace sub))
        | Tau.TCon _ -> ty
      in
      replace sub ty
  ;;
end

(* let rec typecheck_expr ~(ctx : ctx) (e : Ast.expr) : (tau, error) Result.t =
  match e with
  | Ast.Apply (_, _) -> _
  | Ast.BinOp (_, _, _) -> _
  | Ast.Binding (_, _, _) -> _
  | Ast.Group e -> _
  | Ast.Id x -> _
  | Ast.Int _ -> _
  | Ast.Lambda (_, _) -> _
  | Ast.List _ -> _
  | Ast.Match (_, _) -> _
  | Ast.Tuple es -> _
;;*)
