open! Base

type t = (Type_var.t, Type.t, Type_var.comparator_witness) Map.t

let empty : t = Map.empty (module Type_var)

let rec apply_type ~sub ty =
  match ty with
  | Type.TInt -> Type.TInt
  | Type.TVar x ->
    (match Map.find sub x with
     | Some ty' -> ty'
     | None -> ty)
  | Type.TFun (ty0, ty1) -> TFun (apply_type ~sub ty0, apply_type ~sub ty1)
  | Type.TProd tys -> TProd (List.map tys ~f:(fun ty -> apply_type ~sub ty))
  | Type.TCon (tvar, tys) -> TCon (tvar, List.map tys ~f:(fun ty -> apply_type ~sub ty))
;;

let apply_scheme ~sub sc =
  let (Scheme.Forall (qs, ty)) = sc in
  let sub =
    Map.filter_keys sub ~f:(fun tv -> List.exists qs ~f:(Type_var.equal tv) |> not)
  in
  Scheme.Forall (qs, apply_type ~sub ty)
;;

let apply_term_env ~sub env = Term_env.map env ~f:(fun sc -> apply_scheme ~sub sc)

let add sub ~tv ~ty =
  match ty with
  | Type.TVar tv' ->
    if Type_var.equal tv tv'
    then Result.Ok sub
    else Result.Ok (Map.set sub ~key:tv ~data:ty)
  | _ ->
    if Type.occurs tv ty
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
  | Type.TVar tv, ty | ty, Type.TVar tv -> add empty ~tv ~ty
  | Type.TFun (ty_l0, ty_r0), TFun (ty_l1, ty_r1) ->
    let%bind sub = unify ty_l0 ty_l1 in
    let%map sub' = unify (apply_type ~sub ty_r0) (apply_type ~sub ty_r1) in
    compose sub sub'
  | Type.TProd tys, Type.TProd tys' ->
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
  | Type.TCon (x, tys), Type.TCon (x', tys') when String.equal x x' ->
    unify (Type.TProd tys) (Type.TProd tys')
  | _ -> Result.Error "Type unification failed"
;;
