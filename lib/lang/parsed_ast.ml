include Ast.Make (struct
    type var_ident = string
    type ctor_ident = string
    type type_ident = string
    type tvar = string

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
        type for_group = unit
        type for_lambda = unit
        type for_let = unit
        type for_match = unit
        type for_tuple = unit
        type for_binop = unit
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
      end

      module Prog = struct
        type for_prog = unit
      end
    end
  end)
