include Ast.Make (struct
    type var_ident = Var_ident.t
    type ctor_ident = Constr_ident.t
    type type_ident = Type_ident.t
    type tvar = int

    module Ext = struct
      module Pat = struct
        type for_int = unit
        type for_ident = unit
        type for_tuple = unit
        type for_constr = unit
      end

      module Expr = struct
        type for_int = unit
        type for_ident = unit
        type for_constr = unit
        type for_apply = unit
        type for_group = Ast.void
        type for_lambda = unit
        type for_let = unit
        type for_match = unit
        type for_tuple = unit
        type for_binop = Ast.void
      end

      module Ty = struct
        type for_var = unit
        type for_con = unit
        type for_prod = unit
        type for_fun = unit
      end

      module Decl = struct
        type for_val = unit
        type for_type = unit
        type for_ctor = unit
      end

      module Prog = struct
        type for_prog = unit
      end
    end
  end)
