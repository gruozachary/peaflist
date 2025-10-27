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
    | Forall (qs, t) -> Set.diff (Set.of_list (module String) qs) (Tau.free_tvars t)
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
  let introduce env tid sc = Map.add_exn env ~key:tid ~data:sc
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

  let fresh s =
    let v = s.next in
    s.next <- v + 1;
    Tau.TVar ("_'_tvar" ^ Int.to_string v)
  ;;
end

module Subst = struct
  type t = (alpha, Tau.t, String.comparator_witness) Map.t

  let rec apply_type ~sub t =
    match t with
    | Tau.TVar x ->
      (match Map.find sub x with
       | Some t' -> t'
       | None -> t)
    | Tau.TFun (t'0, t'1) -> TFun (apply_type ~sub t'0, apply_type ~sub t'1)
    | Tau.TProd t's -> TProd (List.map t's ~f:(apply_type ~sub))
    | Tau.TApp (tvar, t's) -> TApp (tvar, List.map t's ~f:(apply_type ~sub))
    | Tau.TCon _ -> t

  and apply_scheme ~sub sc =
    let (Scheme.Forall (fa, t)) = sc in
    let sub' = Map.filter_keys sub ~f:(fun x -> List.exists fa ~f:(equal_string x)) in
    Scheme.Forall (fa, apply_type ~sub:sub' t)

  and apply_gamma ~sub env = Gamma.map env ~f:(apply_scheme ~sub)

  let add ~sub tvar t =
    match t with
    | Tau.TVar x ->
      if equal_string x tvar
      then Result.Ok sub
      else Result.Ok (Map.add_exn sub ~key:tvar ~data:t)
    | _ ->
      if Tau.occurs tvar t
      then Result.Error "Recursive type variable definiton"
      else Result.Ok (Map.add_exn sub ~key:tvar ~data:t)
  ;;

  let rec unify ~sub t0 t1 =
    let open Result.Let_syntax in
    let t0' = apply_type ~sub t0 in
    let t1' = apply_type ~sub t1 in
    match t0', t1' with
    | TVar x, t | t, TVar x -> add ~sub x t
    | TCon c0, TCon c1 when Tau.equal_concrete c0 c1 -> Result.Ok sub
    | TFun (l0, r0), TFun (l1, r1) ->
      let%bind sub' = unify ~sub l0 l1 in
      unify ~sub:sub' (apply_type ~sub:sub' r0) (apply_type ~sub:sub' r1)
    | TProd ts0, TProd ts1 ->
      (match
         List.fold2
           ~init:(Result.Ok sub)
           ~f:(fun res_sub t'0 t'1 ->
             let%bind sub = res_sub in
             unify ~sub t'0 t'1)
           ts0
           ts1
       with
       | List.Or_unequal_lengths.Ok x -> x
       | List.Or_unequal_lengths.Unequal_lengths ->
         Result.Error "Tuple arity must be the same")
    | TApp (_tvar0, _ts0), TApp (_tvar1, _ts1) -> assert false (* TODO: implement *)
    | _ -> assert false
  ;;
end

type error = string

type ctx =
  { env : Gamma.t
  ; state : State.t
  ; subst : Subst.t
  }

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
