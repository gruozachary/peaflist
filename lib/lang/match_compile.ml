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

let rec convert_expr : Core_ast.Unified.Expr.t -> (Expr.t, string) Result.t =
  let module O = Core_ast.Unified.Expr in
  let open Expr in
  let convert_exprs exprs =
    let%map exprs =
      List.fold_result exprs ~init:[] ~f:(fun acc expr ->
        let%map expr = convert_expr expr in
        expr :: acc)
    in
    List.rev exprs
  in
  fun expr ->
    match expr with
    | O.Int (x, _) -> return (Int x)
    | O.Ident (ident, ty) -> return (Ident (ident, ty))
    | O.Constr (ident, exprs, ty) ->
      let%map exprs = convert_exprs exprs in
      Constr (ident, exprs, ty)
    | O.Apply (expr_fun, expr_arg, ty) ->
      let%bind expr_fun = convert_expr expr_fun in
      let%map expr_arg = convert_expr expr_arg in
      Apply (expr_fun, expr_arg, ty)
    | O.Lambda (ident, expr_body, ty) ->
      let%map expr_body = convert_expr expr_body in
      Lambda (ident, expr_body, ty)
    | O.Let (ident, expr_binding, expr_body, scheme) ->
      let%bind expr_binding = convert_expr expr_binding in
      let%map expr_body = convert_expr expr_body in
      Let (ident, expr_binding, expr_body, scheme)
    | O.Match (expr_scrutinee, arms, ty) -> _
    | O.Tuple (exprs, ty) ->
      let%map exprs = convert_exprs exprs in
      Tuple (exprs, ty)
    | _ -> .
;;

let convert_decl : Core_ast.Unified.Decl.t -> (Decl.t, string) Result.t = function
  | Core_ast.Unified.Decl.Val (ident, expr, scheme) ->
    let%map expr = convert_expr expr in
    Decl.Val (ident, expr, scheme)
  | Core_ast.Unified.Decl.Type (ident, gen_vars, ctors, type_data) ->
    return (Decl.Type (ident, gen_vars, ctors, type_data))
;;

let convert_prog : Core_ast.Unified.Prog.t -> (Prog.t, string) Result.t =
  fun (Core_ast.Unified.Prog.Decls (decls, ())) ->
  let%map decls =
    List.fold_result
      ~init:[]
      ~f:(fun acc decl ->
        let%map decl = convert_decl decl in
        decl :: acc)
      decls
  in
  Prog.Decls (List.rev decls, ())
;;
