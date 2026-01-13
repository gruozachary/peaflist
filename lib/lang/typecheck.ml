open! Base
open Result
open Let_syntax
module Gen_var = Typing.Gen_var
module Uni_var = Typing.Make_var ()

type ty =
  | TUni of tu ref
  | TGen of Gen_var.t
  | TFun of ty * ty
  | TProd of ty list
  | TCon of Type_ident.t * ty list

and tu =
  | Unbound of Uni_var.t
  | Link of ty

type scheme = Forall of Gen_var.t list * ty

let rec prune = function
  | TUni ({ contents = Link ty } as tu_ref) ->
    let root = prune ty in
    tu_ref := Link root;
    root
  | ty -> ty
;;

let rec unify ty1 ty2 =
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
  | TUni tu_ref, TUni tu_ref' when phys_equal tu_ref tu_ref' -> return ()
  | TUni tu_ref, ty | ty, TUni tu_ref -> return (tu_ref := Link ty)
  | TFun (ty_fun, ty_arg), TFun (ty_fun', ty_arg') ->
    let%bind _ = unify ty_fun ty_fun' in
    unify ty_arg ty_arg'
  | TProd tys, TProd tys' -> unify_lists tys tys'
  | TCon (ident, tys), TCon (ident', tys') when Type_ident.equal ident ident' ->
    unify_lists tys tys'
  | TGen gen_var, TGen gen_var' when Gen_var.equal gen_var gen_var' -> return ()
  | _ -> fail "Type unification failed"
;;

let rec type_of_ty (ty : ty) =
  match prune ty with
  | TUni { contents = Link ty } -> type_of_ty ty
  | TUni { contents = Unbound _ } ->
    raise_s [%message "Internal compiler error: Attempt to convert ununified type"]
  | TGen gen_var -> Typing.Type.Gen gen_var
  | TFun (ty_arg, ty_res) -> Typing.Type.Fun (type_of_ty ty_arg, type_of_ty ty_res)
  | TProd tys -> Typing.Type.Prod (List.map ~f:type_of_ty tys)
  | TCon (ident, tys) -> Typing.Type.Con (ident, List.map ~f:type_of_ty tys)
;;

let scheme_of_scheme (Forall (gen_vars, ty)) =
  Typing.Scheme.Forall (gen_vars, type_of_ty ty)
;;

type tenv_entry =
  { arity : int
  ; ctors : Constr_ident.t list
  }

type cenv_entry =
  { parent : Type_ident.t
  ; tag : int
  ; gen_vars : Gen_var.t list
  ; arg_gens : ty list
  ; res_gen : ty
  }

type ctx =
  { mutable next_gen_var : Gen_var.t
  ; mutable next_uni_var : Uni_var.t
  ; env : (Var_ident.t, scheme, Var_ident.comparator_witness) Map.t
  ; tenv : (Type_ident.t, tenv_entry, Type_ident.comparator_witness) Map.t
  ; cenv : (Constr_ident.t, cenv_entry, Type_ident.comparator_witness) Map.t
  }

let term_fetch ctx ident =
  match Map.find ctx.env ident with
  | Some ty -> ty
  | None -> raise_s [%message "Internal compiler error: Variable out of scope"]
;;

let type_fetch ctx ident =
  match Map.find ctx.tenv ident with
  | Some entry -> entry
  | None -> raise_s [%message "Internal compiler error: Type out of scope"]
;;

let ctor_fetch ctx ident =
  match Map.find ctx.cenv ident with
  | Some entry -> entry
  | None -> raise_s [%message "Internal compiler error: Constructor out of scope"]
;;

let rec free_in_ty ty =
  match ty with
  | TUni { contents = Unbound uni_var } -> Set.singleton (module Uni_var) uni_var
  | TUni { contents = Link _ } -> free_in_ty (prune ty)
  | TGen _ -> Set.empty (module Uni_var)
  | TFun (ty_fun, ty_arg) -> Set.union (free_in_ty ty_fun) (free_in_ty ty_arg)
  | TProd tys -> List.map tys ~f:free_in_ty |> Set.union_list (module Uni_var)
  | TCon (_, tys) -> List.map tys ~f:free_in_ty |> Set.union_list (module Uni_var)
  | _ -> .
;;

let free_in_env =
  Map.fold
    ~init:(Set.empty (module Uni_var))
    ~f:(fun ~key:_ ~data:(Forall (_, ty)) acc -> Set.union acc (free_in_ty ty))
;;

let nullary_type_of_str (rename : Rename.t) str =
  match Rename.Renamer.fetch ~str rename.type_renamer with
  | Some ident -> TCon (ident, [])
  | None -> raise_s [%message "Internal compiler error: Unknown type"]
;;

let fresh_gen_var ctx =
  let x = ctx.next_gen_var in
  ctx.next_gen_var <- Gen_var.succ x;
  x
;;

let scheme_of_ty ty = Forall ([], ty)

let fresh_tu_ref ctx =
  let x = ctx.next_uni_var in
  ctx.next_uni_var <- Uni_var.succ x;
  TUni (Ref.create (Unbound x))
;;

let instantiate_multi ctx gen_vars tys =
  let map =
    List.map ~f:(fun gen_var -> gen_var, fresh_tu_ref ctx) gen_vars
    |> Map.of_alist_exn (module Gen_var)
  in
  let rec replace = function
    | TUni _ as ty -> ty
    | TGen gen_var as ty -> Map.find map gen_var |> Option.value ~default:ty
    | TFun (ty_fun, ty_arg) -> TFun (replace ty_fun, replace ty_arg)
    | TProd tys -> TProd (List.map ~f:replace tys)
    | TCon (ident, tys) -> TCon (ident, List.map ~f:replace tys)
  in
  List.map ~f:replace tys
;;

let instantiate ctx (Forall (gen_vars, ty)) =
  instantiate_multi ctx gen_vars [ ty ] |> List.hd_exn
;;

let generalise ctx ty =
  let free_uni_vars = Set.diff (free_in_ty ty) (free_in_env ctx.env) in
  let map =
    Set.fold
      ~init:(Map.empty (module Uni_var))
      ~f:(fun acc uni_var -> Map.set acc ~key:uni_var ~data:(fresh_gen_var ctx))
      free_uni_vars
  in
  let rec replace = function
    | TUni { contents = Unbound uni_var } as ty ->
      Map.find map uni_var
      |> Option.map ~f:(fun gen_var -> TGen gen_var)
      |> Option.value ~default:ty
    | TUni { contents = Link _ } as ty -> replace (prune ty)
    | TGen _ as ty -> ty
    | TFun (ty_fun, ty_arg) -> TFun (replace ty_fun, replace ty_arg)
    | TProd tys -> TProd (List.map ~f:replace tys)
    | TCon (ident, tys) -> TCon (ident, List.map ~f:replace tys)
  in
  Forall (Map.data map, replace ty)
;;

include Ast.Make (struct
    type var_ident = Var_ident.t
    type ctor_ident = Constr_ident.t
    type type_ident = Type_ident.t
    type tvar = Gen_var.t

    module Ext = struct
      module Pat = struct
        type for_int = unit
        type for_ident = ty
        type for_tuple = ty
        type for_constr = ty
      end

      module Expr = struct
        type for_int = unit
        type for_ident = ty
        type for_constr = ty
        type for_apply = ty
        type for_group = Ast.void
        type for_lambda = ty
        type for_let = scheme
        type for_match = ty
        type for_tuple = ty
        type for_binop = Ast.void
      end

      module Ty = struct
        type for_var = ty
        type for_con = ty
        type for_prod = ty
        type for_fun = ty
      end

      module Decl = struct
        type for_val = scheme
        type for_type = unit
      end

      module Prog = struct
        type for_prog = unit
      end
    end
  end)

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
      return (Ident (ident, ty), Map.singleton (module Var_ident) ident (Forall ([], ty)))
    | O.Tuple (pats, ()) ->
      let%map pats, env = infer_list pats in
      Tuple (pats, TProd (List.map ~f:(ty_of rename) pats)), env
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

  let rec to_core (pat : t) =
    let module N = Core_ast.Pat in
    match pat with
    | Int (x, ()) -> N.Int (x, ())
    | Ident (ident, ty) -> N.Ident (ident, type_of_ty ty)
    | Tuple (pats, ty) -> N.Tuple (List.map ~f:to_core pats, type_of_ty ty)
    | Constr (ident, pats, ty) -> N.Constr (ident, List.map ~f:to_core pats, type_of_ty ty)
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
      let%map () = unify ty_fun (TFun (ty_arg, ty_res)) in
      Apply (expr_fun, expr_arg, ty_res)
    | O.Lambda (ident, expr_body, ()) ->
      let ty_arg = fresh_tu_ref ctx in
      let scheme_arg = Forall ([], ty_arg) in
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
      Tuple (exprs, TProd (List.map ~f:(ty_of rename) exprs))
    | _ -> .
  ;;

  let rec to_core (expr : t) =
    let module N = Core_ast.Expr in
    match expr with
    | Int (x, ()) -> N.Int (x, ())
    | Ident (ident, ty) -> N.Ident (ident, type_of_ty ty)
    | Constr (ident, exprs, ty) ->
      N.Constr (ident, List.map ~f:to_core exprs, type_of_ty ty)
    | Apply (expr_fun, expr_arg, ty) ->
      N.Apply (to_core expr_fun, to_core expr_arg, type_of_ty ty)
    | Lambda (ident, expr_body, ty) -> N.Lambda (ident, to_core expr_body, type_of_ty ty)
    | Let (ident, expr_binding, expr_body, scheme) ->
      N.Let (ident, to_core expr_binding, to_core expr_body, scheme_of_scheme scheme)
    | Match (expr_scrutinee, arms, ty) ->
      N.Match
        ( to_core expr_scrutinee
        , List.map arms ~f:(fun (pat, expr) -> Pat.to_core pat, to_core expr)
        , type_of_ty ty )
    | Tuple (exprs, ty) -> N.Tuple (List.map ~f:to_core exprs, type_of_ty ty)
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
       | Some gen_var -> Var (gen_var, TGen gen_var)
       | None ->
         raise_s
           [%message "Internal compiler error: Mapping from int to gen var doesn't exist"])
    | O.Con (ident, tys, ()) ->
      let tys = List.map tys ~f:(infer tvar_map) in
      Con (ident, tys, TCon (ident, List.map ~f:ty_of tys))
    | O.Prod (tys, ()) ->
      let tys = List.map tys ~f:(infer tvar_map) in
      Prod (tys, TProd (List.map ~f:ty_of tys))
    | O.Fun (ty_fun, ty_arg, ()) ->
      let ty_fun = infer tvar_map ty_fun in
      let ty_arg = infer tvar_map ty_arg in
      Fun (ty_fun, ty_arg, TFun (ty_of ty_fun, ty_of ty_arg))
  ;;

  let rec to_core (ty : t) =
    let module N = Core_ast.Ty in
    match ty with
    | Var (gen_var, ty) -> N.Var (gen_var, type_of_ty ty)
    | Con (ident, tys, ty) -> N.Con (ident, List.map ~f:to_core tys, type_of_ty ty)
    | Prod (tys, ty) -> N.Prod (List.map ~f:to_core tys, type_of_ty ty)
    | Fun (ty_arg, ty_res, ty) -> N.Fun (to_core ty_arg, to_core ty_res, type_of_ty ty)
  ;;
end

module Decl = struct
  include Decl

  let infer (ctx : ctx) (rename : Rename.t) (decl : Desugared_ast.Decl.t) =
    let module O = Desugared_ast.Decl in
    let flat_ty_of_opt_ty ty_opt =
      match Option.map ~f:Ty.ty_of ty_opt with
      | Some (TProd tys) -> tys
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
      let tenv =
        Map.set ctx.tenv ~key:ident ~data:{ arity = List.length tvars; ctors = [] }
      in
      let tvar_map =
        List.map tvars ~f:(fun tvar -> tvar, fresh_gen_var ctx)
        |> Map.of_alist_exn (module Int)
      in
      let gen_vars = Map.data tvar_map in
      let res_gen = TCon (ident, List.map ~f:(fun gen_var -> TGen gen_var) gen_vars) in
      let%map _, ctors, ctx =
        List.fold
          ctors
          ~init:(return (0, [], { ctx with tenv }))
          ~f:(fun acc (ident_ctor, ty_ctor) ->
            let%map i, ctors, ctx = acc in
            let ty_ctor = Option.map ~f:(Ty.infer tvar_map) ty_ctor in
            ( i + 1
            , (ident_ctor, ty_ctor) :: ctors
            , { ctx with
                cenv =
                  Map.set
                    ctx.cenv
                    ~key:ident_ctor
                    ~data:
                      { parent = ident
                      ; tag = i
                      ; gen_vars
                      ; arg_gens = flat_ty_of_opt_ty ty_ctor
                      ; res_gen
                      }
              } ))
      in
      Type (ident, gen_vars, List.rev ctors, ()), ctx
  ;;

  let to_core (decl : t) =
    let module N = Core_ast.Decl in
    match decl with
    | Val (ident, expr, scheme) ->
      N.Val (ident, Expr.to_core expr, scheme_of_scheme scheme)
    | Type (ident, tvars, ctors, ()) ->
      N.Type
        ( ident
        , tvars
        , List.map ctors ~f:(fun (ident, ty_opt) ->
            ident, Option.map ~f:Ty.to_core ty_opt)
        , () )
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

  let to_core (Decls (decls, ())) =
    let module N = Core_ast.Prog in
    N.Decls (List.map ~f:Decl.to_core decls, ())
  ;;
end
