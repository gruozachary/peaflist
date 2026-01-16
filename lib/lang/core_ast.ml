open! Base
open Typing

module Make (M : sig
    type tag
  end) =
struct
  type type_data =
    { arity : int
    ; ctors : Constr_ident.t list
    }

  type ctor_data =
    { parent : Type_ident.t
    ; tag : int
    ; gen_vars : Gen_var.t list
    ; arg_gens : M.tag Type.t list
    ; res_gen : M.tag Type.t
    }

  include Ast.Make (struct
      type var_ident = Var_ident.t
      type ctor_ident = Constr_ident.t
      type type_ident = Type_ident.t
      type tvar = Gen_var.t

      module Pat = struct
        type for_int = unit
        type for_ident = M.tag Type.t
        type for_tuple = M.tag Type.t
        type for_constr = M.tag Type.t
      end

      module Expr = struct
        type for_int = unit
        type for_ident = M.tag Type.t
        type for_constr = M.tag Type.t
        type for_apply = M.tag Type.t
        type for_group = Ast.void
        type for_lambda = M.tag Type.t
        type for_let = M.tag Scheme.t
        type for_match = M.tag Type.t
        type for_tuple = M.tag Type.t
        type for_binop = Ast.void
      end

      module Ty = struct
        type for_var = M.tag Type.t
        type for_con = M.tag Type.t
        type for_prod = M.tag Type.t
        type for_fun = M.tag Type.t
      end

      module Decl = struct
        type for_val = M.tag Scheme.t
        type for_type = type_data
        type for_ctor = ctor_data
      end

      module Prog = struct
        type for_decls = unit
      end
    end)
end

module Unified = Make (struct
    type tag = Type.phantom_unified
  end)
