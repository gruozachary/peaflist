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

let rec replace_type_vars m ty =
  match ty with
  | Type.TVar tv ->
    (match Map.find m tv with
     | Some tv' -> Type.TVar tv'
     | None -> ty)
  | Type.TFun (ty0, ty1) -> TFun (replace_type_vars m ty0, replace_type_vars m ty1)
  | Type.TProd tys -> TProd (List.map tys ~f:(replace_type_vars m))
  | Type.TCon (tvar, tys) -> TCon (tvar, List.map tys ~f:(replace_type_vars m))
;;

let instantiate ctx = function
  | Scheme.Forall (qs, ty) ->
    let sub =
      List.fold
        ~init:(Map.empty (module Type_var))
        ~f:(fun acc key ->
          Map.set acc ~key ~data:(Analyser_ctx.State.get ctx |> Analyser_state.fresh_tv))
        qs
    in
    replace_type_vars sub ty
;;

let instantiate_two ctx (Scheme.Forall (qs_1, ty_1)) (Scheme.Forall (qs_2, ty_2)) =
  let rec go m ~xs ~ys =
    match xs, ys with
    | [], [] -> m
    | x :: xs, [] | [], x :: xs ->
      Map.set m ~key:x ~data:(Analyser_ctx.State.get ctx |> Analyser_state.fresh_tv)
      |> go ~xs ~ys:[]
    | x :: xs, y :: ys ->
      let tv = Analyser_ctx.State.get ctx |> Analyser_state.fresh_tv in
      Map.set m ~key:x ~data:tv |> Map.set ~key:y ~data:tv |> go ~xs ~ys
  in
  let m = go (Map.empty (module Type_var)) ~xs:qs_1 ~ys:qs_2 in
  replace_type_vars m ty_1, replace_type_vars m ty_2
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
    | CtorApp of id * t list
  [@@deriving eq]

  let rec infer ctx p =
    let open Result.Let_syntax in
    let empty_ident_renamer =
      Renamer.empty (Analyser_ctx.State.get ctx |> Analyser_state.ident_renamer_heart)
    in
    match p with
    | CtorApp (ident_str, ps) ->
      let%bind ident, entry =
        Analyser_ctx.constr_fetch_and_lookup ctx ~ident_str
        |> Result.of_option ~error:"Unbound constructor"
      in
      let%map s, g, r, t, ps_c =
        match entry.arg_scheme_opt with
        | Option.None ->
          let ty_res = instantiate ctx entry.res_scheme in
          return (Subst.empty, Term_env.empty (), empty_ident_renamer, ty_res, [])
        | Option.Some arg_scheme ->
          let ty_param, ty_res = instantiate_two ctx arg_scheme entry.res_scheme in
          let%map s, g, r, ps_c =
            match ps with
            | [ p ] ->
              let%bind s_arg, g_arg, r_arg, ty_arg, p_c = infer ctx p in
              let%map s = Subst.unify ty_arg ty_param >>| Subst.compose s_arg in
              s, g_arg, r_arg, [ p_c ]
            | ps ->
              let%bind s_arg, g_arg, r_arg, ty_arg, p_c = infer ctx (Tuple ps) in
              let%map s = Subst.unify ty_arg ty_param >>| Subst.compose s_arg in
              ( s
              , g_arg
              , r_arg
              , (match p_c with
                 | Core.Pat.Tuple (ps_c, _) -> ps_c
                 | _ -> raise_s [%message "Typecheck wasn't tuple"]) )
          in
          let t = Subst.apply_type ~sub:s ty_res in
          s, g, r, t, ps_c
      in
      s, g, r, t, Core.Pat.CtorApp (ident, ps_c, t)
    | Ident ident_str ->
      let t = Analyser_ctx.State.get ctx |> Analyser_state.fresh in
      let ident, r =
        empty_ident_renamer
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
        , empty_ident_renamer
        , make_prim_tcon ctx "int"
        , Core.Pat.Int x )
    | Tuple ps ->
      let%map s, g, r, ts, p_cs =
        List.fold
          ~init:(Result.Ok (Subst.empty, Term_env.empty (), empty_ident_renamer, [], []))
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
    | Constr (ident_str, es) ->
      let%bind ident, entry =
        Analyser_ctx.constr_fetch_and_lookup ctx ~ident_str
        |> Result.of_option ~error:"Unbound constructor"
      in
      let%map s, t, es_c =
        match entry.arg_scheme_opt with
        | Option.None ->
          let ty_res = instantiate ctx entry.res_scheme in
          return (Subst.empty, ty_res, [])
        | Option.Some arg_scheme ->
          let ty_param, ty_res = instantiate_two ctx arg_scheme entry.res_scheme in
          let%map s, es_c =
            match es with
            | [ e ] ->
              let%bind s_arg, ty_arg, e_c = infer ctx e in
              let%map s = Subst.unify ty_arg ty_param >>| Subst.compose s_arg in
              s, [ e_c ]
            | es ->
              let%bind s_arg, ty_arg, e_c = infer ctx (Tuple es) in
              let%map s = Subst.unify ty_arg ty_param >>| Subst.compose s_arg in
              ( s
              , (match e_c with
                 | Core.Expr.Tuple (es_c, _) -> es_c
                 | _ -> raise_s [%message "Typecheck wasn't tuple"]) )
          in
          let t = Subst.apply_type ~sub:s ty_res in
          s, t, es_c
      in
      s, t, Core.Expr.Constr (ident, es_c, t)
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
                     ( Analyser_ctx.Var_ident_renamer.get ctx
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
              Analyser_ctx.Var_ident_renamer.map ctx ~f:(fun x -> Renamer.merge r x)
              |> Analyser_ctx.Env.map
                   ~f:
                     (Term_env.merge g ~f:(fun ~key:_ m ->
                        match m with
                        | `Left x -> Option.Some x
                        | `Right x -> Option.Some x
                        | `Both (_, r) -> Option.Some r))
            in
            let%map s_exp, t_exp, e_c = infer ctx e in
            ( Subst.compose s s_exp
            , Subst.apply_type ~sub:s t_exp :: ts
            , (p_c, e_c) :: arms_c ))
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
    let%map s, ctx, d =
      match d with
      | ValDecl (ident_str, e) ->
        let%map s, ty, e_c = Expr.infer ctx e in
        let ctx = Analyser_ctx.Env.map ctx ~f:(Subst.apply_term_env ~sub:s) in
        let scheme = generalise ctx ty in
        let ident, ctx = Analyser_ctx.declare_and_introduce ctx ~ident_str ~scheme in
        s, ctx, Core.Decl.ValDecl (ident, e_c)
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
               ~heart:
                 (Analyser_ctx.State.get ctx |> Analyser_state.type_ident_renamer_heart)
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
              let%bind tv_map = get_tvs () in
              let tvs = Map.data tv_map in
              let tyvs = List.map ~f:(fun tv -> Type.TVar tv) tvs in
              let%map arg_scheme_opt, res_scheme =
                match t_opt with
                | Option.Some t ->
                  (match
                     Ty.to_type
                       ~renamer:(Analyser_ctx.Type_ident_renamer.get ctx)
                       ~vm:tv_map
                       t
                   with
                   | Option.Some ty ->
                     return
                       ( Option.Some (Scheme.Forall (tvs, ty))
                       , Scheme.Forall (tvs, Type.TCon (ident_parent, tyvs)) )
                   | Option.None -> Result.Error "Unbound type variable")
                | Option.None ->
                  return (Option.None, Scheme.Forall (tvs, Type.TCon (ident_parent, tyvs)))
              in
              let ident_constr, ctx =
                Analyser_ctx.constr_declare_and_introduce
                  ctx
                  ~ident_str
                  ~entry:{ parent = ident_parent; tag; arg_scheme_opt; res_scheme }
              in
              ctx, ident_constr :: constrs, tag + 1)
        in
        let ctx =
          Analyser_ctx.Tenv.map
            ctx
            ~f:(Type_env.introduce ~id:ident_parent ~data:{ arity; constrs })
        in
        Subst.empty, ctx, Core.Decl.TypeDecl ident_parent
    in
    ctx, Core.Decl.zonk s d
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
