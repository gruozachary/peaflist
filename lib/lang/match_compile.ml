open! Base
open Typing

module Ast = struct
  module Expr = struct
    type t =
      | Int of int
      | Ident of Var_ident.t * Type.unified_t
      | Constr of Constr_ident.t * t List.t * Type.unified_t
      | Apply of t * t * Type.unified_t
      | Lambda of Var_ident.t * t * Type.unified_t
      | Let of Var_ident.t * t * t * Scheme.unified_t
      | Tuple of t List.t * Type.unified_t
      | GetTag of Var_ident.t
      | GetField of Var_ident.t * int
      | Switch of t * (int * t) * t option * Type.unified_t
  end

  module Ty = Core_ast.Unified.Ty

  module Decl = Ast.Make_decl (struct
      module Expr = Expr
      module Ty = Ty

      type var_ident = Var_ident.t
      type type_ident = Type_ident.t
      type ctor_ident = Constr_ident.t
      type tvar = Gen_var.t
      type for_val = Scheme.unified_t
      type for_type = Core_ast.Unified.type_data
      type for_ctor = Core_ast.Unified.ctor_data
    end)

  module Prog = Ast.Make_prog (struct
      module Decl = Decl

      type for_decls = unit
    end)
end

open Ast
open Result.Let_syntax

type ctx =
  { tenv : (Type_ident.t, Core_ast.Unified.type_data, Type_ident.comparator_witness) Map.t
  ; cenv :
      (Constr_ident.t, Core_ast.Unified.ctor_data, Constr_ident.comparator_witness) Map.t
  }

module Compilation = struct
  type ctor_tag = int
  type pattern = Core_ast.Unified.Pat.t
  type action = Core_ast.Unified.Expr.t
  type row = pattern list * action
  type matrix = row list

  type tree =
    | Fail
    | Leaf of action
    | Swap of int * tree
    | Switch of (ctor_tag * tree) list * tree option

  let is_pat_wildcard : pattern -> bool = function
    | Core_ast.Unified.Pat.Ident _ -> true
    | _ -> false
  ;;

  let all_pats_wildcard : row -> bool =
    fun (pats, _) -> List.for_all ~f:is_pat_wildcard pats
  ;;

  let find_column : matrix -> int =
    fun mat ->
    match mat with
    | [] -> assert false
    | (pats, _) :: _ ->
      List.find_mapi_exn pats ~f:(fun ix pat ->
        if is_pat_wildcard pat then None else Some ix)
  ;;

  let swap_to_front_row : int -> row -> row =
    fun i (pats, action) ->
    let xs, ys = List.split_n pats i in
    if equal_int i 1
    then pats, action
    else List.append (List.last_exn xs :: List.drop_last_exn xs) ys, action
  ;;

  let swap_to_front : int -> matrix -> matrix =
    fun ix mat -> List.map ~f:(swap_to_front_row ix) mat
  ;;

  let rec compile_match : matrix -> tree = function
    | [] -> Fail
    | ((_, action) as row) :: _ when all_pats_wildcard row -> Leaf action
    | mat ->
      let col_index = find_column mat in
      let transform_node, mat =
        if equal_int col_index 0
        then (fun t -> Swap (col_index, t)), swap_to_front col_index mat
        else (fun t -> t), mat
      in
      let specialised =
        List.map mat ~f:(fun (pats, _) ->
          let pat = List.hd_exn pats in
          get_ctor_tag pat, specialise mat pat)
      in
      transform_node (Switch (specialised, _))

  and specialise : matrix -> pattern -> tree =
    fun mat pat ->
    let ctor_tag = get_ctor_tag pat in
    let mat_s =
      List.filter mat ~f:(fun (pats, _) -> equal_int (List.hd_exn pats) ctor_tag)
      |> List.map ~f:(fun (pats, action) ->
        let prefix =
          match List.hd_exn pats with
          | Core_ast.Unified.Pat.Int _ -> [ pat ]
          | Core_ast.Unified.Pat.Ident _ -> [ pat ]
          | Core_ast.Unified.Pat.Tuple (pats, _) -> pats
          | Core_ast.Unified.Pat.Constr (_, pats, _) -> pats
        in
        List.append prefix (List.tl_exn pats), action)
    in
    compile_match mat_s
  ;;
end

let rec convert_expr : ctx -> Core_ast.Unified.Expr.t -> (Expr.t, string) Result.t =
  fun ctx ->
  let module O = Core_ast.Unified.Expr in
  let open Expr in
  let convert_exprs exprs =
    let%map exprs =
      List.fold_result exprs ~init:[] ~f:(fun acc expr ->
        let%map expr = convert_expr ctx expr in
        expr :: acc)
    in
    List.rev exprs
  in
  function
  | O.Int (x, _) -> return (Int x)
  | O.Ident (ident, ty) -> return (Ident (ident, ty))
  | O.Constr (ident, exprs, ty) ->
    let%map exprs = convert_exprs exprs in
    Constr (ident, exprs, ty)
  | O.Apply (expr_fun, expr_arg, ty) ->
    let%bind expr_fun = convert_expr ctx expr_fun in
    let%map expr_arg = convert_expr ctx expr_arg in
    Apply (expr_fun, expr_arg, ty)
  | O.Lambda (ident, expr_body, ty) ->
    let%map expr_body = convert_expr ctx expr_body in
    Lambda (ident, expr_body, ty)
  | O.Let (ident, expr_binding, expr_body, scheme) ->
    let%bind expr_binding = convert_expr ctx expr_binding in
    let%map expr_body = convert_expr ctx expr_body in
    Let (ident, expr_binding, expr_body, scheme)
  | O.Match (expr_scrutinee, arms, ty) -> _
  | O.Tuple (exprs, ty) ->
    let%map exprs = convert_exprs exprs in
    Tuple (exprs, ty)
  | _ -> .
;;

let convert_decl : ctx -> Core_ast.Unified.Decl.t -> (Decl.t * ctx, string) Result.t =
  fun ctx -> function
  | Core_ast.Unified.Decl.Val (ident, expr, scheme) ->
    let%map expr = convert_expr ctx expr in
    Decl.Val (ident, expr, scheme), ctx
  | Core_ast.Unified.Decl.Type (ident, gen_vars, ctors, type_data) ->
    let tenv = Map.set ctx.tenv ~key:ident ~data:type_data in
    let cenv =
      List.fold
        ~init:ctx.cenv
        ~f:(fun cenv (ident_c, _, ctor_data) -> Map.set cenv ~key:ident_c ~data:ctor_data)
        ctors
    in
    return (Decl.Type (ident, gen_vars, ctors, type_data), { tenv; cenv })
;;

let convert_prog : ctx -> Core_ast.Unified.Prog.t -> (Prog.t * ctx, string) Result.t =
  fun ctx (Core_ast.Unified.Prog.Decls (decls, ())) ->
  let%map decls, ctx =
    List.fold_result
      ~init:([], ctx)
      ~f:(fun (decls_acc, ctx) decl ->
        let%map decl, ctx = convert_decl ctx decl in
        decl :: decls_acc, ctx)
      decls
  in
  Prog.Decls (List.rev decls, ()), ctx
;;
