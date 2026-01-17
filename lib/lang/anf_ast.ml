open! Base
open Typing

module Expr = struct
  type atomic
  type compound

  type _ t' =
    | Int : Int.t -> atomic t'
    | Var : Var_ident.t * Type.unified_t -> atomic t'
    | Lambda : Var_ident.t * Type.unified_t * t -> atomic t'
    | Constr : Constr_ident.t * atomic t' List.t * Type.unified_t -> compound t'
    | Apply : atomic t' * atomic t' * Type.unified_t -> compound t'
    | Let : Var_ident.t * Scheme.unified_t * t * t -> compound t'
    | Tuple : atomic t' List.t * Type.unified_t -> compound t'

  and t = Any : 'a t' -> t
end

module Ty = Core_ast.Unified.Ty

module Decl = Ast.Make_decl (struct
    module Expr = Expr
    module Ty = Ty

    type var_ident = Var_ident.t
    type type_ident = Type_ident.t
    type ctor_ident = Constr_ident.t
    type tvar = int
    type for_val = Scheme.unified_t
    type for_type = Core_ast.Unified.type_data
    type for_ctor = Core_ast.Unified.ctor_data
  end)

module Prog = Ast.Make_prog (struct
    module Decl = Decl

    type for_decls = unit
  end)
