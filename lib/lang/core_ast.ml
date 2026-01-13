open! Base
open Typing

include Ast.Make (struct
    type var_ident = Var_ident.t
    type ctor_ident = Constr_ident.t
    type type_ident = Type_ident.t
    type tvar = Gen_var.t

    module Ext = struct
      module Pat = struct
        type for_int = unit
        type for_ident = Type.t
        type for_tuple = Type.t
        type for_constr = Type.t
      end

      module Expr = struct
        type for_int = unit
        type for_ident = Type.t
        type for_constr = Type.t
        type for_apply = Type.t
        type for_group = Ast.void
        type for_lambda = Type.t
        type for_let = Scheme.t
        type for_match = Type.t
        type for_tuple = Type.t
        type for_binop = Ast.void
      end

      module Ty = struct
        type for_var = Type.t
        type for_con = Type.t
        type for_prod = Type.t
        type for_fun = Type.t
      end

      module Decl = struct
        type for_val = Scheme.t
        type for_type = unit
      end

      module Prog = struct
        type for_prog = unit
      end
    end
  end)
