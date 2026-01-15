open! Base
open Result
open Let_syntax
open Typing

include Core_ast.Make (struct
    type tag = Type.phantom_ununified
  end)

let rec prune = function
  | Type.Uni ({ contents = Type.Link ty } as tu_ref) ->
    let root = prune ty in
    tu_ref := Link root;
    root
  | ty -> ty
;;

let rec unify ty1 ty2 =
  let open Type in
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
  | Uni tu_ref, Uni tu_ref' when phys_equal tu_ref tu_ref' -> return ()
  | Uni tu_ref, ty | ty, Uni tu_ref -> return (tu_ref := Link ty)
  | Fun (ty_fun, ty_arg), Fun (ty_fun', ty_arg') ->
    let%bind _ = unify ty_fun ty_fun' in
    unify ty_arg ty_arg'
  | Prod tys, Prod tys' -> unify_lists tys tys'
  | Con (ident, tys), Con (ident', tys') when Type_ident.equal ident ident' ->
    unify_lists tys tys'
  | Gen gen_var, Gen gen_var' when Gen_var.equal gen_var gen_var' -> return ()
  | _ -> fail "Type unification failed"
;;

type ctx =
  { mutable next_gen_var : Gen_var.t
  ; mutable next_uni_var : Uni_var.t
  ; env : (Var_ident.t, Scheme.ununified_t, Var_ident.comparator_witness) Map.t
  ; tenv : (Type_ident.t, type_data, Type_ident.comparator_witness) Map.t
  ; cenv : (Constr_ident.t, ctor_data, Constr_ident.comparator_witness) Map.t
  }

let empty () =
  { next_gen_var = Gen_var.zero
  ; next_uni_var = Uni_var.zero
  ; env = Map.empty (module Var_ident)
  ; tenv = Map.empty (module Type_ident)
  ; cenv = Map.empty (module Constr_ident)
  }
;;

let basic (rename : Rename.t) =
  let open Type in
  let vident str = Rename.Renamer.fetch_exn rename.var_renamer ~str in
  let tident str = Rename.Renamer.fetch_exn rename.type_renamer ~str in
  let ctx = empty () in
  let two_int_fun =
    Scheme.Forall
      ( []
      , Fun (Con (tident "int", []), Fun (Con (tident "int", []), Con (tident "int", [])))
      )
  in
  let env =
    Map.of_alist_exn
      (module Var_ident)
      [ vident "+", two_int_fun
      ; vident "-", two_int_fun
      ; vident "*", two_int_fun
      ; vident "/", two_int_fun
      ]
  in
  let tenv =
    Map.of_alist_exn (module Type_ident) [ tident "int", { arity = 0; ctors = [] } ]
  in
  { ctx with env; tenv }
;;

let term_fetch ctx ident =
  match Map.find ctx.env ident with
  | Some ty -> ty
  | None -> raise_s [%message "Internal compiler error: Variable out of scope"]
;;

let ctor_fetch ctx ident =
  match Map.find ctx.cenv ident with
  | Some entry -> entry
  | None -> raise_s [%message "Internal compiler error: Constructor out of scope"]
;;

let rec free_in_ty ty =
  let open Type in
  match ty with
  | Uni { contents = Unbound uni_var } -> Set.singleton (module Uni_var) uni_var
  | Uni { contents = Link _ } -> free_in_ty (prune ty)
  | Gen _ -> Set.empty (module Uni_var)
  | Fun (ty_fun, ty_arg) -> Set.union (free_in_ty ty_fun) (free_in_ty ty_arg)
  | Prod tys -> List.map tys ~f:free_in_ty |> Set.union_list (module Uni_var)
  | Con (_, tys) -> List.map tys ~f:free_in_ty |> Set.union_list (module Uni_var)
  | _ -> .
;;

let free_in_env =
  Map.fold
    ~init:(Set.empty (module Uni_var))
    ~f:(fun ~key:_ ~data:(Scheme.Forall (_, ty)) acc -> Set.union acc (free_in_ty ty))
;;

let nullary_type_of_str (rename : Rename.t) str =
  match Rename.Renamer.fetch ~str rename.type_renamer with
  | Some ident -> Type.Con (ident, [])
  | None -> raise_s [%message "Internal compiler error: Unknown type"]
;;

let fresh_gen_var ctx =
  let x = ctx.next_gen_var in
  ctx.next_gen_var <- Gen_var.succ x;
  x
;;

let fresh_tu_ref ctx =
  let x = ctx.next_uni_var in
  ctx.next_uni_var <- Uni_var.succ x;
  Type.Uni (Ref.create (Type.Unbound x))
;;

let instantiate_multi ctx gen_vars tys =
  let open Type in
  let map =
    List.map ~f:(fun gen_var -> gen_var, fresh_tu_ref ctx) gen_vars
    |> Map.of_alist_exn (module Gen_var)
  in
  let rec replace = function
    | Uni _ as ty -> ty
    | Gen gen_var as ty -> Map.find map gen_var |> Option.value ~default:ty
    | Fun (ty_fun, ty_arg) -> Fun (replace ty_fun, replace ty_arg)
    | Prod tys -> Prod (List.map ~f:replace tys)
    | Con (ident, tys) -> Con (ident, List.map ~f:replace tys)
  in
  List.map ~f:replace tys
;;

let instantiate ctx (Scheme.Forall (gen_vars, ty)) =
  instantiate_multi ctx gen_vars [ ty ] |> List.hd_exn
;;

let generalise ctx ty =
  let open Type in
  let free_uni_vars = Set.diff (free_in_ty ty) (free_in_env ctx.env) in
  let map =
    Set.fold
      ~init:(Map.empty (module Uni_var))
      ~f:(fun acc uni_var -> Map.set acc ~key:uni_var ~data:(fresh_gen_var ctx))
      free_uni_vars
  in
  let rec replace = function
    | Uni ({ contents = Unbound uni_var } as tu_ref) as ty ->
      (match Map.find map uni_var with
       | Some gen_var ->
         tu_ref := Link (Gen gen_var);
         Gen gen_var
       | None -> ty)
    | Uni { contents = Link _ } as ty -> replace (prune ty)
    | Gen _ as ty -> ty
    | Fun (ty_fun, ty_arg) -> Fun (replace ty_fun, replace ty_arg)
    | Prod tys -> Prod (List.map ~f:replace tys)
    | Con (ident, tys) -> Con (ident, List.map ~f:replace tys)
  in
  Scheme.Forall (Map.data map, replace ty)
;;

let rec ty_clean : Type.ununified_t -> Type.unified_t =
  fun ty ->
  match prune ty with
  | Uni { contents = Link ty } -> ty_clean ty
  | Uni { contents = Unbound _ } ->
    raise_s [%message "Internal compiler error: Attempt to convert ununified type"]
  | Gen gen_var -> Typing.Type.Gen gen_var
  | Fun (ty_arg, ty_res) -> Typing.Type.Fun (ty_clean ty_arg, ty_clean ty_res)
  | Prod tys -> Typing.Type.Prod (List.map ~f:ty_clean tys)
  | Con (ident, tys) -> Typing.Type.Con (ident, List.map ~f:ty_clean tys)
;;

let scheme_clean : Scheme.ununified_t -> Scheme.unified_t =
  fun (Scheme.Forall (gen_vars, ty)) -> Scheme.Forall (gen_vars, ty_clean ty)
;;

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
      return
        ( Ident (ident, ty)
        , Map.singleton (module Var_ident) ident (Scheme.Forall ([], ty)) )
    | O.Tuple (pats, ()) ->
      let%map pats, env = infer_list pats in
      Tuple (pats, Type.Prod (List.map ~f:(ty_of rename) pats)), env
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

  let rec clean (pat : t) =
    let module N = Core_ast.Unified.Pat in
    match pat with
    | Int (x, ()) -> N.Int (x, ())
    | Ident (ident, ty) -> N.Ident (ident, ty_clean ty)
    | Tuple (pats, ty) -> N.Tuple (List.map ~f:clean pats, ty_clean ty)
    | Constr (ident, pats, ty) -> N.Constr (ident, List.map ~f:clean pats, ty_clean ty)
  ;;
end

module Expr = struct
  include Expr

  let rec ty_of (rename : Rename.t) (expr : t) =
    match expr with
    | Int (_, ()) -> nullary_type_of_str rename "int"
    | Ident (_, ty) -> ty
    | Constr (_, _, ty) -> ty
    | Apply (_, _, ty) -> ty
    | Lambda (_, expr_body, _) -> ty_of rename expr_body
    | Let (_, _, expr_body, _) -> ty_of rename expr_body
    | Match (_, _, ty) -> ty
    | Tuple (_, ty) -> ty
    | _ -> .
  ;;

  let rec infer (ctx : ctx) (rename : Rename.t) (expr : Desugared_ast.Expr.t) =
    let module O = Desugared_ast.Expr in
    let overwrite_merge map map' =
      Map.merge map map' ~f:(fun ~key:_ -> function
        | `Right y -> Some y
        | `Left x -> Some x
        | `Both (_, y) -> Some y)
    in
    let infer_list exprs =
      let%map exprs =
        List.fold
          ~init:(return [])
          ~f:(fun acc expr ->
            let%bind exprs_acc = acc in
            let%map expr_inf = infer ctx rename expr in
            expr_inf :: exprs_acc)
          exprs
      in
      List.rev exprs
    in
    match expr with
    | O.Int (x, ()) -> return (Int (x, ()))
    | O.Ident (ident, ()) ->
      let ty = term_fetch ctx ident |> instantiate ctx in
      return (Ident (ident, ty))
    | O.Constr (ident, exprs, ()) ->
      let entry = ctor_fetch ctx ident in
      let arg_tys, res_ty =
        match instantiate_multi ctx entry.gen_vars (entry.res_gen :: entry.arg_gens) with
        | res_ty :: arg_tys -> arg_tys, res_ty
        | _ -> assert false
      in
      let%bind exprs_inf = infer_list exprs in
      let%bind zipped =
        match List.zip arg_tys (List.map ~f:(ty_of rename) exprs_inf) with
        | List.Or_unequal_lengths.Ok zipped -> return zipped
        | List.Or_unequal_lengths.Unequal_lengths -> fail "Arity for constructor wrong"
      in
      let%map () =
        List.fold zipped ~init:(return ()) ~f:(fun acc (ty, ty') ->
          let%bind () = acc in
          unify ty ty')
      in
      Constr (ident, exprs_inf, res_ty)
    | O.Apply (expr_fun, expr_arg, ()) ->
      let%bind expr_fun = infer ctx rename expr_fun in
      let%bind expr_arg = infer ctx rename expr_arg in
      let ty_fun = ty_of rename expr_fun in
      let ty_arg = ty_of rename expr_arg in
      let ty_res = fresh_tu_ref ctx in
      let%map () = unify ty_fun (Type.Fun (ty_arg, ty_res)) in
      Apply (expr_fun, expr_arg, ty_res)
    | O.Lambda (ident, expr_body, ()) ->
      let ty_arg = fresh_tu_ref ctx in
      let scheme_arg = Scheme.Forall ([], ty_arg) in
      let%map expr_body =
        infer
          { ctx with env = Map.set ctx.env ~key:ident ~data:scheme_arg }
          rename
          expr_body
      in
      Lambda (ident, expr_body, ty_arg)
    | O.Let (ident, expr_binding, expr_body, ()) ->
      let%bind expr_binding = infer ctx rename expr_binding in
      let scheme_binding = generalise ctx (ty_of rename expr_binding) in
      let%map expr_body =
        infer
          { ctx with env = Map.set ctx.env ~key:ident ~data:scheme_binding }
          rename
          expr_body
      in
      Let (ident, expr_binding, expr_body, scheme_binding)
    | O.Match (expr_scrutinee, arms, ()) ->
      let%bind expr_scrutinee = infer ctx rename expr_scrutinee in
      let ty = fresh_tu_ref ctx in
      let%map arms =
        List.fold arms ~init:(return []) ~f:(fun acc (pat, expr) ->
          let%bind arms_acc = acc in
          let%bind pat, env_inf = Pat.infer ctx rename pat in
          let%bind expr =
            infer { ctx with env = overwrite_merge ctx.env env_inf } rename expr
          in
          let%map () = unify ty (ty_of rename expr) in
          (pat, expr) :: arms_acc)
      in
      Match (expr_scrutinee, List.rev arms, ty)
    | O.Tuple (exprs, ()) ->
      let%map exprs = infer_list exprs in
      Tuple (exprs, Type.Prod (List.map ~f:(ty_of rename) exprs))
    | _ -> .
  ;;

  let rec clean (expr : t) =
    let module N = Core_ast.Unified.Expr in
    match expr with
    | Int (x, ()) -> N.Int (x, ())
    | Ident (ident, ty) -> N.Ident (ident, ty_clean ty)
    | Constr (ident, exprs, ty) -> N.Constr (ident, List.map ~f:clean exprs, ty_clean ty)
    | Apply (expr_fun, expr_arg, ty) ->
      N.Apply (clean expr_fun, clean expr_arg, ty_clean ty)
    | Lambda (ident, expr_body, ty) -> N.Lambda (ident, clean expr_body, ty_clean ty)
    | Let (ident, expr_binding, expr_body, scheme) ->
      N.Let (ident, clean expr_binding, clean expr_body, scheme_clean scheme)
    | Match (expr_scrutinee, arms, ty) ->
      N.Match
        ( clean expr_scrutinee
        , List.map arms ~f:(fun (pat, expr) -> Pat.clean pat, clean expr)
        , ty_clean ty )
    | Tuple (exprs, ty) -> N.Tuple (List.map ~f:clean exprs, ty_clean ty)
    | _ -> .
  ;;
end

module Ty = struct
  include Ty

  let ty_of = function
    | Var (_, ty) -> ty
    | Con (_, _, ty) -> ty
    | Prod (_, ty) -> ty
    | Fun (_, _, ty) -> ty
  ;;

  let rec infer
            (tvar_map : (int, Gen_var.t, Int.comparator_witness) Map.t)
            (ty : Desugared_ast.Ty.t)
    =
    let module O = Desugared_ast.Ty in
    match ty with
    | O.Var (x, _) ->
      (match Map.find tvar_map x with
       | Some gen_var -> Var (gen_var, Type.Gen gen_var)
       | None ->
         raise_s
           [%message "Internal compiler error: Mapping from int to gen var doesn't exist"])
    | O.Con (ident, tys, ()) ->
      let tys = List.map tys ~f:(infer tvar_map) in
      Con (ident, tys, Type.Con (ident, List.map ~f:ty_of tys))
    | O.Prod (tys, ()) ->
      let tys = List.map tys ~f:(infer tvar_map) in
      Prod (tys, Type.Prod (List.map ~f:ty_of tys))
    | O.Fun (ty_fun, ty_arg, ()) ->
      let ty_fun = infer tvar_map ty_fun in
      let ty_arg = infer tvar_map ty_arg in
      Fun (ty_fun, ty_arg, Type.Fun (ty_of ty_fun, ty_of ty_arg))
  ;;

  let rec clean (ty : t) =
    let module N = Core_ast.Unified.Ty in
    match ty with
    | Var (gen_var, ty) -> N.Var (gen_var, ty_clean ty)
    | Con (ident, tys, ty) -> N.Con (ident, List.map ~f:clean tys, ty_clean ty)
    | Prod (tys, ty) -> N.Prod (List.map ~f:clean tys, ty_clean ty)
    | Fun (ty_arg, ty_res, ty) -> N.Fun (clean ty_arg, clean ty_res, ty_clean ty)
  ;;
end

module Decl = struct
  include Decl

  let infer (ctx : ctx) (rename : Rename.t) (decl : Desugared_ast.Decl.t) =
    let module O = Desugared_ast.Decl in
    let flat_ty_of_opt_ty ty_opt =
      match Option.map ~f:Ty.ty_of ty_opt with
      | Some (Type.Prod tys) -> tys
      | Some ty -> [ ty ]
      | _ -> []
    in
    match decl with
    | O.Val (ident, expr, ()) ->
      let%map expr = Expr.infer ctx rename expr in
      let scheme = Expr.ty_of rename expr |> generalise ctx in
      ( Val (ident, expr, scheme)
      , { ctx with env = Map.set ctx.env ~key:ident ~data:scheme } )
    | O.Type (ident, tvars, ctors, ()) ->
      let tenv_entry = { arity = List.length tvars; ctors = [] } in
      let tenv = Map.set ctx.tenv ~key:ident ~data:tenv_entry in
      let tvar_map =
        List.map tvars ~f:(fun tvar -> tvar, fresh_gen_var ctx)
        |> Map.of_alist_exn (module Int)
      in
      let gen_vars = Map.data tvar_map in
      let res_gen =
        Type.Con (ident, List.map ~f:(fun gen_var -> Type.Gen gen_var) gen_vars)
      in
      let%map _, ctors, ctx =
        List.fold
          ctors
          ~init:(return (0, [], { ctx with tenv }))
          ~f:(fun acc (ident_ctor, ty_ctor, ()) ->
            let%map i, ctors, ctx = acc in
            let ty_ctor = Option.map ~f:(Ty.infer tvar_map) ty_ctor in
            let ctor_entry =
              { parent = ident
              ; tag = i
              ; gen_vars
              ; arg_gens = flat_ty_of_opt_ty ty_ctor
              ; res_gen
              }
            in
            ( i + 1
            , (ident_ctor, ty_ctor, ctor_entry) :: ctors
            , { ctx with cenv = Map.set ctx.cenv ~key:ident_ctor ~data:ctor_entry } ))
      in
      Type (ident, gen_vars, List.rev ctors, tenv_entry), ctx
  ;;

  let clean (decl : t) =
    let module N = Core_ast.Unified.Decl in
    match decl with
    | Val (ident, expr, scheme) -> N.Val (ident, Expr.clean expr, scheme_clean scheme)
    | Type (ident, tvars, ctors, tenv_entry) ->
      N.Type
        ( ident
        , tvars
        , List.map ctors ~f:(fun (ident, ty_opt, cenv_entry) ->
            let (ctor_data : Core_ast.Unified.ctor_data) =
              { parent = cenv_entry.parent
              ; tag = cenv_entry.tag
              ; gen_vars = cenv_entry.gen_vars
              ; arg_gens = List.map ~f:ty_clean cenv_entry.arg_gens
              ; res_gen = ty_clean cenv_entry.res_gen
              }
            in
            ident, Option.map ~f:Ty.clean ty_opt, ctor_data)
        , { arity = tenv_entry.arity; ctors = tenv_entry.ctors } )
  ;;
end

module Prog = struct
  include Prog

  let infer
        (ctx : ctx)
        (rename : Rename.t)
        (Desugared_ast.Prog.Decls (decls, ()) : Desugared_ast.Prog.t)
    =
    let module O = Desugared_ast.Prog in
    let%map decls, ctx =
      List.fold
        decls
        ~init:(return ([], ctx))
        ~f:(fun acc decl ->
          let%bind decls_acc, ctx = acc in
          let%map decl, ctx = Decl.infer ctx rename decl in
          decl :: decls_acc, ctx)
    in
    Decls (List.rev decls, ()), ctx
  ;;

  let clean (Decls (decls, ())) =
    let module N = Core_ast.Unified.Prog in
    N.Decls (List.map ~f:Decl.clean decls, ())
  ;;
end

let typecheck_expr ctx rename expr = Expr.infer ctx rename expr >>| Expr.clean

let typecheck_decl ctx rename decl =
  let%map decl, ctx = Decl.infer ctx rename decl in
  Decl.clean decl, ctx
;;

let typecheck_prog ctx rename prog =
  let%map prog, ctx = Prog.infer ctx rename prog in
  Prog.clean prog, ctx
;;
