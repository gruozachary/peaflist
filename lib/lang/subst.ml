open! Base

type t = (Tvar.t, Tau.t, Tvar.comparator_witness) Map.t

let empty : t = Map.empty (module Tvar)

let rec apply_type ~sub ty =
  match ty with
  | Tau.TVar x ->
    (match Map.find sub x with
     | Some ty' -> ty'
     | None -> ty)
  | Tau.TFun (ty0, ty1) -> TFun (apply_type ~sub ty0, apply_type ~sub ty1)
  | Tau.TProd tys -> TProd (List.map tys ~f:(fun ty -> apply_type ~sub ty))
  | Tau.TCon (tvar, tys) -> TCon (tvar, List.map tys ~f:(fun ty -> apply_type ~sub ty))
;;

let apply_scheme ~sub sc =
  let (Scheme.Forall (qs, ty)) = sc in
  let sub = Map.filter_keys sub ~f:(fun tv -> List.exists qs ~f:(Tvar.equal tv) |> not) in
  Scheme.Forall (qs, apply_type ~sub ty)
;;

let apply_term_env ~sub env = Term_env.map env ~f:(fun sc -> apply_scheme ~sub sc)

let add sub ~tv ~ty =
  match ty with
  | Tau.TVar tv' ->
    if Tvar.equal tv tv' then Result.Ok sub else Result.Ok (Map.set sub ~key:tv ~data:ty)
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
    let%map sub' = unify (apply_type ~sub ty_r0) (apply_type ~sub ty_r1) in
    compose sub sub'
  | Tau.TProd tys, Tau.TProd tys' ->
    (match
       List.fold2
         ~init:(Result.Ok empty)
         ~f:(fun res_sub ty'0 ty'1 ->
           let%bind sub = res_sub in
           let%map sub' = unify (apply_type ~sub ty'0) (apply_type ~sub ty'1) in
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
