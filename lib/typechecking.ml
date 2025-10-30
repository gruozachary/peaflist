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

  let of_type ty = Forall ([], ty)
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
    | Tau.TVar tv, ty | ty, Tau.TVar tv -> add ~sub:empty tv ty
    | Tau.TCon c0, TCon c1 when Tau.equal_concrete c0 c1 -> Result.Ok empty
    | Tau.TFun (ty_l0, ty_r0), TFun (ty_l1, ty_r1) ->
      let%bind sub = unify ty_l0 ty_l1 in
      let%map sub' = unify (apply_type ~sub ty_r0) (apply_type ~sub ty_r1) in
      compose sub sub'
    | Tau.TProd tys, Tau.TProd tys' ->
      (match
         List.fold2
           ~init:(Result.Ok empty)
           ~f:(fun res_sub t'0 t'1 ->
             let%bind sub = res_sub in
             let%map sub' = unify t'0 t'1 in
             compose sub sub')
           tys
           tys'
       with
       | List.Or_unequal_lengths.Ok x -> x
       | List.Or_unequal_lengths.Unequal_lengths ->
         Result.Error "Tuple arity must be the same")
    | _ -> Result.Error "Type unification failed"
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

  let generalise ctx ty =
    let env_tvs = Gamma.free_tvars ctx.env in
    let ty_tvs = Tau.free_tvars ty in
    Scheme.Forall (Set.diff ty_tvs env_tvs |> Set.to_list, ty)
  ;;

  let rec expr ctx e =
    let open Result.Let_syntax in
    match e with
    | Ast.Id x ->
      (match Gamma.lookup ctx.env x with
       | Option.Some sc ->
         let ty = instantiate ctx sc in
         Result.Ok (Subst.empty, ty)
       | Option.None -> Result.Error "Unbound variable")
    | Ast.Lambda (x, e) ->
      let ty = State.fresh ctx.state in
      let ctx' = { ctx with env = Gamma.introduce ctx.env x (Scheme.of_type ty) } in
      let%map sub, ty' = expr ctx' e in
      sub, Subst.apply_type ~sub ty'
    | Ast.Apply (ef, e) ->
      let%bind sub0, tyf = expr ctx ef in
      let%bind sub1, ty = expr { ctx with env = Subst.apply_gamma ~sub:sub0 ctx.env } e in
      let tyv = State.fresh ctx.state in
      let%map sub2 = Subst.unify (Subst.apply_type ~sub:sub1 tyf) (Tau.TFun (ty, tyv)) in
      Subst.compose (Subst.compose sub0 sub1) sub2, Subst.apply_type ~sub:sub2 tyv
    | Ast.Binding (x, e0, e1) ->
      let%bind s0, ty0 = expr ctx e0 in
      let ctx' = { ctx with env = Subst.apply_gamma ~sub:s0 ctx.env } in
      let sc = generalise ctx' ty0 in
      let%map s1, ty1 = expr { ctx' with env = Gamma.introduce ctx.env x sc } e1 in
      Subst.compose s0 s1, ty1
    | Ast.Group e -> expr ctx e
    | Ast.Int _ -> Result.Ok (Subst.empty, Tau.TCon Tau.Int)
    | Ast.BinOp (_, _, _) -> Result.Error "Bin op typechecking not implemented"
    | Ast.Match (_, _) -> Result.Error "Match typechecking not implemented"
    | Ast.List _ -> Result.Error "List typechecking not implemented"
    | Ast.Tuple _ -> Result.Error "Tuple typechecking not implemented"
  ;;
end
