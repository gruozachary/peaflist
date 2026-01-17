open! Base
open Typing

module Ast = struct
  module Expr = struct
    type t =
      | Int of int * unit
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

let convert_expr : Core_ast.Unified.Expr.t -> (Expr.t, string) Result.t = _

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
  Prog.Decls (decls, ())
;;
