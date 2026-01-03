open! Base
open Lang

module Expr = struct
  type atom =
    | Int of Int.t
    | Var of Var_ident.t * Type.t
    | Lambda of Var_ident.t * Type.t * compound

  and compound =
    | Constr of Constr_ident.t * atom List.t * Type.t
    | Apply of atom * atom * Type.t
    | Let of Var_ident.t * Scheme.t * compound * compound
    | Tuple of atom List.t
  (* match will be done later *)
end
