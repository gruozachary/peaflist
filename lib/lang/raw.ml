open! Base

type id = string [@@deriving eq]
type ty_id = string [@@deriving eq]
type ty_var = string [@@deriving eq]

let fetch_type_ident_exn ctx ~ident_str =
  Analyser_ctx.Type_ident_renamer.get ctx
  |> Renamer.fetch ~str:ident_str
  |> Option.value_exn
;;

let make_prim_tcon ctx str = Type.TCon (fetch_type_ident_exn ctx ~ident_str:str, [])

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
    | CtorApp (ident_str, po) ->
      let%bind ident =
        Analyser_ctx.Ident_renamer.get ctx
        |> Renamer.fetch ~str:ident_str
        |> Result.of_option ~error:"Constructor name unrecognised"
      in
      let%bind t =
        Result.of_option
          ~error:"Unbound constructor in pattern"
          (Option.map
             (Analyser_ctx.Env.get ctx |> Term_env.lookup ~id:ident)
             ~f:(instantiate ctx))
      in
      (match t, po with
       | Type.TFun _, Option.Some p ->
         let%bind s, g, r, t', p_c = infer ctx p in
         let t'' = Analyser_ctx.State.get ctx |> Analyser_state.fresh in
         let%map s' = Subst.unify (Type.TFun (t', t'')) t in
         let s'' = Subst.compose s s' in
         ( s''
         , g
         , r
         , Subst.apply_type ~sub:s'' t''
         , Core.Pat.CtorApp (ident, Option.Some p_c, t'') )
       | Type.TCon _, Option.None ->
         Result.Ok
           ( Subst.empty
           , Term_env.empty ()
           , Renamer.empty
               (Analyser_ctx.State.get ctx |> Analyser_state.ident_renamer_heart)
           , t
           , Core.Pat.CtorApp (ident, Option.None, t) )
       | _ -> Result.Error "Constructor arity mismatch in pattern")
    | Ident ident_str ->
      let t = Analyser_ctx.State.get ctx |> Analyser_state.fresh in
      let ident, r =
        Analyser_ctx.Ident_renamer.get ctx
        |> Renamer.declare_and_fetch
             ~str:ident_str
             ~heart:(Analyser_ctx.State.get ctx |> Analyser_state.ident_renamer_heart)
      in
      Result.Ok
        ( Subst.empty
        , Term_env.introduce ~id:ident ~sc:(Scheme.of_type t) (Term_env.empty ())
        , r
        , t
        , Core.Pat.Ident (ident, t) )
    | Int x ->
      Result.Ok
        ( Subst.empty
        , Term_env.empty ()
        , Renamer.empty (Analyser_ctx.State.get ctx |> Analyser_state.ident_renamer_heart)
        , make_prim_tcon ctx "int"
        , Core.Pat.Int x )
    | Tuple ps ->
      let%map s, g, r, ts, p_cs =
        List.fold
          ~init:
            (Result.Ok
               ( Subst.empty
               , Term_env.empty ()
               , Renamer.empty
                   (Analyser_ctx.State.get ctx |> Analyser_state.ident_renamer_heart)
               , []
               , [] ))
          ~f:(fun sgo p ->
            let%bind s, g, r, ts, p_cs = sgo in
            let%bind s', g', r', t, p_c = infer ctx p in
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
            ( Subst.compose s s'
            , g''
            , Renamer.merge r r'
            , t :: List.map ~f:(Subst.apply_type ~sub:s') ts
            , p_c :: p_cs ))
          ps
      in
      let t = Type.TProd (List.rev ts) in
      s, g, r, t, Core.Pat.Tuple (List.rev p_cs, t)
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
    | Constr of id * t list
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
    | Id ident_str ->
      (match Analyser_ctx.fetch_and_lookup ctx ~ident_str with
       | Option.Some (ident, sc) ->
         let ty = instantiate ctx sc in
         Result.Ok (Subst.empty, ty, Core.Expr.Id (ident, ty))
       | Option.None -> Result.Error "Unbound variable")
    | Constr ident_str ->
      (match Analyser_ctx.fetch_and_lookup ctx ~ident_str with
       | Option.Some (ident, sc) ->
         let ty = instantiate ctx sc in
         Result.Ok (Subst.empty, ty, Core.Expr.Constr (ident, ty))
       | Option.None -> Result.Error "Unbound constructor")
    | Lambda (ident_str, e) ->
      let ty = Analyser_ctx.State.get ctx |> Analyser_state.fresh in
      let ident, ctx =
        Analyser_ctx.declare_and_introduce ctx ~ident_str ~scheme:(Scheme.of_type ty)
      in
      let%map s, ty', e_c = infer ctx e in
      ( s
      , Type.TFun (Subst.apply_type ~sub:s ty, Subst.apply_type ~sub:s ty')
      , Core.Expr.Lambda (ident, ty, e_c) )
    | Apply (ef, e) ->
      let%bind s, tyf, ef_c = infer ctx ef in
      let ctx = Analyser_ctx.Env.map ctx ~f:(Subst.apply_term_env ~sub:s) in
      let%bind s', ty, e_c = infer ctx e in
      let tyv = Analyser_ctx.State.get ctx |> Analyser_state.fresh in
      let%map s'' = Subst.unify (Subst.apply_type ~sub:s' tyf) (Type.TFun (ty, tyv)) in
      ( Subst.compose (Subst.compose s s') s''
      , Subst.apply_type ~sub:s'' tyv
      , Core.Expr.Apply (ef_c, e_c, tyv) )
    | Binding (ident_str, e, e') ->
      let%bind s, ty, e_c = infer ctx e in
      let ctx = Analyser_ctx.Env.map ctx ~f:(Subst.apply_term_env ~sub:s) in
      let scheme = generalise ctx ty in
      let ident, ctx = Analyser_ctx.declare_and_introduce ctx ~ident_str ~scheme in
      let%map s', ty', e_c' = infer ctx e' in
      Subst.compose s s', ty', Core.Expr.Let (ident, scheme, e_c, e_c')
    | Group e -> infer ctx e
    | Int x -> Result.Ok (Subst.empty, make_prim_tcon ctx "int", Core.Expr.Int x)
    | BinOp (el, o, er) ->
      let open Bin_op in
      (match o with
       | Div | Mul | Plus | Sub ->
         let%bind s, ty, el_c = infer ctx el in
         let ctx = Analyser_ctx.Env.map ctx ~f:(Subst.apply_term_env ~sub:s) in
         let%bind s1, ty', er_c = infer ctx er in
         let int_tcon = make_prim_tcon ctx "int" in
         let%bind s2 = Subst.unify (Subst.apply_type ~sub:s1 ty) int_tcon in
         let%map s3 = Subst.unify (Subst.apply_type ~sub:s2 ty') int_tcon in
         let s' = Subst.compose (Subst.compose (Subst.compose s s1) s2) s3 in
         ( s'
         , int_tcon
         , Core.Expr.Apply
             ( Core.Expr.Apply
                 ( Core.Expr.Id
                     ( Analyser_ctx.Ident_renamer.get ctx
                       |> Renamer.fetch
                            ~str:
                              (match o with
                               | Div -> "/"
                               | Mul -> "*"
                               | Plus -> "+"
                               | Sub -> "-")
                       |> Option.value_exn
                     , Type.TFun (int_tcon, Type.TFun (int_tcon, int_tcon)) )
                 , el_c
                 , Type.TFun (int_tcon, int_tcon) )
             , er_c
             , int_tcon ) ))
    | Match (o, arms) ->
      let%bind s_opr, t_opr, e_opr_c = infer ctx o in
      let%bind s, ts, arms_c =
        List.fold
          ~init:(Result.Ok (s_opr, [], []))
          arms
          ~f:(fun acc (p, e) ->
            let%bind s, ts, arms_c = acc in
            let%bind s_pat, g, r, t_pat, p_c = Pat.infer ctx p in
            let s = Subst.compose s s_pat in
            let%bind s_uni =
              Subst.unify (Subst.apply_type ~sub:s t_pat) (Subst.apply_type ~sub:s t_opr)
            in
            let s = Subst.compose s s_uni in
            let ctx =
              Analyser_ctx.Ident_renamer.map ctx ~f:(fun x -> Renamer.merge r x)
              |> Analyser_ctx.Env.map
                   ~f:
                     (Term_env.merge g ~f:(fun ~key:_ m ->
                        match m with
                        | `Left x -> Option.Some x
                        | `Right x -> Option.Some x
                        | `Both (_, r) -> Option.Some r))
            in
            let%map s_exp, t_exp, e_c = infer ctx e in
            Subst.compose s s_exp, t_exp :: ts, (p_c, e_c) :: arms_c)
      in
      let%bind t, ts =
        match ts with
        | [] -> Result.Error "Cannot have no match arms"
        | t :: ts -> Result.Ok (t, ts)
      in
      let%map s, t =
        List.fold
          ~init:(Result.Ok (s, Subst.apply_type ~sub:s t))
          ~f:(fun acc t ->
            let%bind s, t_acc = acc in
            let%map s_uni = Subst.unify t_acc (Subst.apply_type ~sub:s t) in
            let s = Subst.compose s s_uni in
            s, Subst.apply_type ~sub:s t_acc)
          ts
      in
      s, t, Core.Expr.Match (e_opr_c, List.rev arms_c, t)
    | Tuple es ->
      let%map s, tys, es_c =
        List.fold
          ~init:(Result.Ok (Subst.empty, [], []))
          ~f:(fun acc e ->
            let%bind s, tys, es_c = acc in
            let ctx = Analyser_ctx.Env.map ctx ~f:(Subst.apply_term_env ~sub:s) in
            let%map s', ty, e_c = infer ctx e in
            Subst.compose s s', ty :: tys, e_c :: es_c)
          es
      in
      let tys = List.map ~f:(Subst.apply_type ~sub:s) tys in
      let t = Type.TProd (List.rev tys) in
      s, t, Core.Expr.Tuple (List.rev es_c, t)
  ;;

  let typecheck ctx e =
    infer ctx e |> Result.map ~f:(fun (sub, t, core) -> t, Core.Expr.zonk sub core)
  ;;
end

module Ty = struct
  type t =
    | Var of id
    | Con of id * t list
    | Prod of t list
    | Fun of t * t
  [@@deriving eq]

  let rec to_type ~renamer ~vm t =
    let open Option.Let_syntax in
    match t with
    | Var x ->
      let%map tv = Map.find vm x in
      Type.TVar tv
    | Fun (t, t') ->
      let%bind ty_l = to_type ~renamer ~vm t in
      let%map ty_r = to_type ~renamer ~vm t' in
      Type.TFun (ty_l, ty_r)
    | Prod ts ->
      let%map tys = Option.all (List.map ~f:(to_type ~renamer ~vm) ts) in
      Type.TProd tys
    | Con (ident_str, ts) ->
      let%bind tys = Option.all (List.map ~f:(to_type ~renamer ~vm) ts) in
      let%map ident = Renamer.fetch renamer ~str:ident_str in
      Type.TCon (ident, tys)
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
    | ValDecl (ident_str, e) ->
      let%map s, ty, e_c = Expr.infer ctx e in
      let ctx = Analyser_ctx.Env.map ctx ~f:(Subst.apply_term_env ~sub:s) in
      let scheme = generalise ctx ty in
      let ident, ctx = Analyser_ctx.declare_and_introduce ctx ~ident_str ~scheme in
      ctx, Core.Decl.ValDecl (ident, e_c)
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
      let ident_parent, type_ident_renamer =
        Analyser_ctx.Type_ident_renamer.get ctx
        |> Renamer.declare_and_fetch
             ~heart:(Analyser_ctx.State.get ctx |> Analyser_state.type_ident_renamer_heart)
             ~str:x
      in
      let ctx =
        Analyser_ctx.Type_ident_renamer.map ctx ~f:(fun _ -> type_ident_renamer)
      in
      let%map ctx, constrs, _ =
        List.fold
          ctors
          ~init:(Result.Ok (ctx, [], 0))
          ~f:(fun ctx_opt (ident_str, t_opt) ->
            let%bind ctx, constrs, tag = ctx_opt in
            let%map tv_map = get_tvs () in
            let tvs = Map.data tv_map in
            let tyvs = List.map ~f:(fun tv -> Type.TVar tv) tvs in
            let scheme =
              match t_opt with
              | Option.Some t ->
                (match
                   Ty.to_type
                     ~renamer:(Analyser_ctx.Type_ident_renamer.get ctx)
                     ~vm:tv_map
                     t
                 with
                 | Option.Some ty ->
                   Scheme.Forall (tvs, Type.TFun (ty, Type.TCon (ident_parent, tyvs)))
                 | Option.None -> assert false)
              | Option.None -> Scheme.Forall (tvs, Type.TCon (ident_parent, tyvs))
            in
            let ident_constr, ctx =
              Analyser_ctx.constr_declare_and_introduce
                ctx
                ~ident_str
                ~entry:{ parent = ident_parent; tag; scheme }
            in
            ctx, ident_constr :: constrs, tag + 1)
      in
      let ctx =
        Analyser_ctx.Tenv.map
          ctx
          ~f:(Type_env.introduce ~id:ident_parent ~data:{ arity; constrs })
      in
      ctx, Core.Decl.TypeDecl ident_parent
  ;;
end

module Prog = struct
  type t = Decl.t list [@@deriving eq]

  let typecheck ctx ds =
    let open Result.Let_syntax in
    let rec go ctx ds_c ~ds =
      match ds with
      | [] -> Result.Ok (ctx, ds_c)
      | d :: ds ->
        let%bind ctx, d_c = Decl.typecheck ctx d in
        go ctx (d_c :: ds_c) ~ds
    in
    let%map ctx, ds_c = go ctx [] ~ds in
    ctx, List.rev ds_c
  ;;
end
