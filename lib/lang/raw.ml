open! Base

type id = string [@@deriving eq]
type ty_id = string [@@deriving eq]
type ty_var = string [@@deriving eq]

let instantiate ctx = function
  | Scheme.Forall (qs, ty) ->
    let sub =
      List.fold
        ~init:(Map.empty (module Type_var))
        ~f:(fun acc key ->
          Map.set acc ~key ~data:(Analyser_ctx.State.get ctx |> Analyser_state.fresh_tv))
        qs
    in
    let rec replace sub ty =
      match ty with
      | Type.TVar tv ->
        (match Map.find sub tv with
         | Some tv' -> Type.TVar tv'
         | None -> ty)
      | Type.TFun (ty0, ty1) -> TFun (replace sub ty0, replace sub ty1)
      | Type.TProd tys -> TProd (List.map tys ~f:(replace sub))
      | Type.TCon (tvar, tys) -> TCon (tvar, List.map tys ~f:(replace sub))
    in
    replace sub ty
;;

let generalise ctx ty =
  let env_tvs = Analyser_ctx.Env.get ctx |> Term_env.free_tvars in
  let ty_tvs = Type.free_tvars ty in
  Scheme.Forall (Set.diff ty_tvs env_tvs |> Set.to_list, ty)
;;

module Pat = struct
  type t =
    | Int of int
    | Ident of id
    | Tuple of t list
    | CtorApp of id * t Option.t
  [@@deriving eq]

  let rec infer ctx p =
    let open Result.Let_syntax in
    match p with
    | CtorApp (x, po) ->
      let%bind t =
        Result.of_option
          ~error:"Unbound constructor in pattern"
          (Option.map
             (Analyser_ctx.Env.get ctx |> Term_env.lookup ~id:x)
             ~f:(instantiate ctx))
      in
      (match t, po with
       | Type.TFun _, Option.Some p ->
         let%bind s, g, t', p_c = infer ctx p in
         let t'' = Analyser_ctx.State.get ctx |> Analyser_state.fresh in
         let%map s' = Subst.unify (Type.TFun (t', t'')) t in
         let s'' = Subst.compose s s' in
         s'', g, Subst.apply_type ~sub:s'' t'', Core.Pat.CtorApp (x, Option.Some p_c, t'')
       | Type.TCon _, Option.None ->
         Result.Ok
           (Subst.empty, Term_env.empty (), t, Core.Pat.CtorApp (x, Option.None, t))
       | _ -> Result.Error "Constructor arity mismatch in pattern")
    | Ident x ->
      let t = Analyser_ctx.State.get ctx |> Analyser_state.fresh in
      Result.Ok
        ( Subst.empty
        , Term_env.introduce ~id:x ~sc:(Scheme.of_type t) (Term_env.empty ())
        , t
        , Core.Pat.Ident (x, t) )
    | Int x -> Result.Ok (Subst.empty, Term_env.empty (), Type.TCon ("int", []), Core.Pat.Int x)
    | Tuple ps ->
      let%map s, g, ts, p_cs =
        List.fold
          ~init:(Result.Ok (Subst.empty, Term_env.empty (), [], []))
          ~f:(fun sgo p ->
            let%bind s, g, ts, p_cs = sgo in
            let%bind s', g', t, p_c = infer ctx p in
            let g = Subst.apply_term_env ~sub:s' g in
            let g'' =
              Term_env.merge g g' ~f:(fun ~key:_ m ->
                match m with
                | `Left x -> Option.Some x
                | `Right x -> Option.Some x
                | `Both _ -> Option.None)
            in
            let%map g'' =
              if equal_int (Term_env.size g'') (Term_env.size g + Term_env.size g')
              then Result.Ok g''
              else Result.Error "Variable redefinition inside of pattern"
            in
            Subst.compose s s', g'', t :: List.map ~f:(Subst.apply_type ~sub:s') ts, p_c :: p_cs)
          ps
      in
      let t = Type.TProd (List.rev ts) in
      s, g, t, Core.Pat.Tuple (List.rev p_cs, t)
  ;;
end

module Expr = struct
  module Bin_op = struct
    type t =
      | Plus
      | Sub
      | Mul
      | Div
    [@@deriving eq]
  end

  type t =
    | Int of int
    | Id of id
    | Constr of id
    | Apply of t * t
    | Group of t
    | Lambda of id * t
    | Binding of id * t * t
    | Match of t * (Pat.t * t) list
    | Tuple of t list
    | BinOp of t * Bin_op.t * t
  [@@deriving eq]

  let rec infer ctx e =
    let open Result.Let_syntax in
    match e with
    | Id x ->
      (match Analyser_ctx.Env.get ctx |> Term_env.lookup ~id:x with
       | Option.Some sc ->
         let ty = instantiate ctx sc in
         Result.Ok (Subst.empty, ty)
       | Option.None -> Result.Error "Unbound variable")
    | Constr x ->
      (match Analyser_ctx.Env.get ctx |> Term_env.lookup ~id:x with
       | Option.Some sc ->
         let ty = instantiate ctx sc in
         Result.Ok (Subst.empty, ty)
       | Option.None -> Result.Error "Unbound constructor")
    | Lambda (x, e) ->
      let ty = Analyser_ctx.State.get ctx |> Analyser_state.fresh in
      let ctx =
        Analyser_ctx.Env.map ctx ~f:(Term_env.introduce ~id:x ~sc:(Scheme.of_type ty))
      in
      let%map s, ty' = infer ctx e in
      s, Type.TFun (Subst.apply_type ~sub:s ty, Subst.apply_type ~sub:s ty')
    | Apply (ef, e) ->
      let%bind s, tyf = infer ctx ef in
      let ctx = Analyser_ctx.Env.map ctx ~f:(Subst.apply_term_env ~sub:s) in
      let%bind s', ty = infer ctx e in
      let tyv = Analyser_ctx.State.get ctx |> Analyser_state.fresh in
      let%map s'' = Subst.unify (Subst.apply_type ~sub:s' tyf) (Type.TFun (ty, tyv)) in
      Subst.compose (Subst.compose s s') s'', Subst.apply_type ~sub:s'' tyv
    | Binding (x, e, e') ->
      let%bind s, ty = infer ctx e in
      let ctx = Analyser_ctx.Env.map ctx ~f:(Subst.apply_term_env ~sub:s) in
      let sc = generalise ctx ty in
      let%map s', ty' =
        infer (Analyser_ctx.Env.map ctx ~f:(Term_env.introduce ~id:x ~sc)) e'
      in
      Subst.compose s s', ty'
    | Group e -> infer ctx e
    | Int _ -> Result.Ok (Subst.empty, Type.TCon ("int", []))
    | BinOp (el, o, er) ->
      let open Bin_op in
      (match o with
       | Div | Mul | Plus | Sub ->
         let%bind s, ty = infer ctx el in
         let ctx = Analyser_ctx.Env.map ctx ~f:(Subst.apply_term_env ~sub:s) in
         let%bind s1, ty' = infer ctx er in
         let%bind s2 =
           Subst.unify (Subst.apply_type ~sub:s1 ty) (Type.TCon ("int", []))
         in
         let%map s3 =
           Subst.unify (Subst.apply_type ~sub:s2 ty') (Type.TCon ("int", []))
         in
         let s' = Subst.compose (Subst.compose (Subst.compose s s1) s2) s3 in
         s', Type.TCon ("int", []))
    | Match (o, arms) ->
      let%bind s_opr, t_opr = infer ctx o in
      let%bind s, ts =
        List.fold
          ~init:(Result.Ok (s_opr, []))
          arms
          ~f:(fun acc (p, e) ->
            let%bind s, ts = acc in
            let%bind s_pat, g, t_pat, _ = Pat.infer ctx p in
            let s = Subst.compose s s_pat in
            let%bind s_uni =
              Subst.unify (Subst.apply_type ~sub:s t_pat) (Subst.apply_type ~sub:s t_opr)
            in
            let s = Subst.compose s s_uni in
            let ctx =
              Analyser_ctx.Env.map
                ctx
                ~f:
                  (Term_env.merge g ~f:(fun ~key:_ m ->
                     match m with
                     | `Left x -> Option.Some x
                     | `Right x -> Option.Some x
                     | `Both (_, r) -> Option.Some r))
            in
            let%map s_exp, t_exp = infer ctx e in
            Subst.compose s s_exp, t_exp :: ts)
      in
      let%bind t, ts =
        match ts with
        | [] -> Result.Error "Cannot have no match arms"
        | t :: ts -> Result.Ok (t, ts)
      in
      List.fold
        ~init:(Result.Ok (s, Subst.apply_type ~sub:s t))
        ~f:(fun acc t ->
          let%bind s, t_acc = acc in
          let%map s_uni = Subst.unify t_acc (Subst.apply_type ~sub:s t) in
          let s = Subst.compose s s_uni in
          s, Subst.apply_type ~sub:s t_acc)
        ts
    | Tuple es ->
      let%map s, tys =
        List.fold
          ~init:(Result.Ok (Subst.empty, []))
          ~f:(fun acc e ->
            let%bind s, tys = acc in
            let ctx = Analyser_ctx.Env.map ctx ~f:(Subst.apply_term_env ~sub:s) in
            let%map s', ty = infer ctx e in
            Subst.compose s s', ty :: tys)
          es
      in
      let tys = List.map ~f:(Subst.apply_type ~sub:s) tys in
      s, Type.TProd (List.rev tys)
  ;;

  let typecheck ctx e = infer ctx e |> Result.map ~f:(fun (_, t) -> t)
end

module Ty = struct
  type t =
    | Var of id
    | Con of id * t list
    | Prod of t list
    | Fun of t * t
  [@@deriving eq]

  let rec to_type ~vm t =
    let open Option.Let_syntax in
    match t with
    | Var x ->
      let%map tv = Map.find vm x in
      Type.TVar tv
    | Fun (t, t') ->
      let%bind ty_l = to_type ~vm t in
      let%map ty_r = to_type ~vm t' in
      Type.TFun (ty_l, ty_r)
    | Prod ts ->
      let%map tys = Option.all (List.map ~f:(to_type ~vm) ts) in
      Type.TProd tys
    | Con (x, ts) ->
      let%map tys = Option.all (List.map ~f:(to_type ~vm) ts) in
      Type.TCon (x, tys)
  ;;
end

module Decl = struct
  type t =
    | ValDecl of id * Expr.t
    | TypeDecl of id * ty_var list * (id * Ty.t option) list
  [@@deriving eq]

  let typecheck ctx d =
    let open Result.Let_syntax in
    match d with
    | ValDecl (x, e) ->
      let%map s, ty = Expr.infer ctx e in
      let ctx = Analyser_ctx.Env.map ctx ~f:(Subst.apply_term_env ~sub:s) in
      let sc = generalise ctx ty in
      Analyser_ctx.Env.map ctx ~f:(Term_env.introduce ~id:x ~sc)
    | TypeDecl (x, utvs, ctors) ->
      let arity = List.length utvs in
      let get_tvs () =
        match
          Map.of_alist
            (module String)
            (List.map
               ~f:(fun tv -> tv, Analyser_ctx.State.get ctx |> Analyser_state.fresh_tv)
               utvs)
        with
        | `Ok x -> Result.Ok x
        | `Duplicate_key _ -> Result.Error "Duplicate type variable"
      in
      let ctx = Analyser_ctx.Tenv.map ctx ~f:(Type_env.introduce ~id:x ~arity) in
      List.fold ctors ~init:(Result.Ok ctx) ~f:(fun ctx_opt (y, t_opt) ->
        let%bind ctx = ctx_opt in
        let%map tv_map = get_tvs () in
        let tvs = Map.data tv_map in
        let tyvs = List.map ~f:(fun tv -> Type.TVar tv) tvs in
        let sc =
          match t_opt with
          | Option.Some t ->
            (match Ty.to_type ~vm:tv_map t with
             | Option.Some ty -> Scheme.Forall (tvs, Type.TFun (ty, Type.TCon (x, tyvs)))
             | Option.None -> assert false)
          | Option.None -> Scheme.Forall (tvs, Type.TCon (x, tyvs))
        in
        Analyser_ctx.Env.map ctx ~f:(Term_env.introduce ~id:y ~sc))
  ;;
end

module Prog = struct
  type t = Decl.t list [@@deriving eq]

  let typecheck ctx ds =
    let open Result in
    let rec go ctx ~ds =
      match ds with
      | [] -> Result.Ok ctx
      | d :: ds -> Decl.typecheck ctx d >>= go ~ds
    in
    go ctx ~ds
  ;;
end
