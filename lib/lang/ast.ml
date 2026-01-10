open! Base

type void = |

module type META = sig
  type var_ident
  type ctor_ident
  type type_ident
  type tvar

  module Ext : sig
    module Pat : sig
      type for_int
      type for_ident
      type for_tuple
      type for_constr
    end

    module Expr : sig
      type for_int
      type for_ident
      type for_constr
      type for_apply
      type for_group
      type for_lambda
      type for_let
      type for_match
      type for_tuple
      type for_binop
    end

    module Ty : sig
      type for_var
      type for_con
      type for_prod
      type for_fun
    end

    module Decl : sig
      type for_val
      type for_type
    end

    module Prog : sig
      type for_prog
    end
  end
end

module Make (M : META) = struct
  module Pat = struct
    type t =
      | Int of int * M.Ext.Pat.for_int
      | Ident of M.var_ident * M.Ext.Pat.for_ident
      | Tuple of t list * M.Ext.Pat.for_tuple
      | Constr of M.ctor_ident * t list * M.Ext.Pat.for_constr
  end

  module Expr = struct
    module Bin_op = struct
      type t =
        [ `Plus
        | `Mul
        | `Sub
        | `Div
        ]
    end

    type t =
      | Int of int * M.Ext.Expr.for_int
      | Ident of M.var_ident * M.Ext.Expr.for_ident
      | Constr of M.ctor_ident * t List.t * M.Ext.Expr.for_constr
      | Apply of t * t * M.Ext.Expr.for_apply
      | Group of t * M.Ext.Expr.for_group
      | Lambda of M.var_ident * t * M.Ext.Expr.for_lambda
      | Let of M.var_ident * t * t * M.Ext.Expr.for_let
      | Match of t * (Pat.t * t) List.t * M.Ext.Expr.for_match
      | Tuple of t List.t * M.Ext.Expr.for_tuple
      | BinOp of t * Bin_op.t * t * M.Ext.Expr.for_binop
  end

  module Ty = struct
    type t =
      | Var of M.tvar * M.Ext.Ty.for_var
      | Con of M.type_ident * t list * M.Ext.Ty.for_con
      | Prod of t list * M.Ext.Ty.for_prod
      | Fun of t * t * M.Ext.Ty.for_fun
  end

  module Decl = struct
    type t =
      | Val of M.var_ident * Expr.t * M.Ext.Decl.for_val
      | Type of
          M.type_ident
          * M.tvar list
          * (M.ctor_ident * Ty.t option) list
          * M.Ext.Decl.for_type
  end

  module Prog = struct
    type t = Decls of Decl.t list * M.Ext.Prog.for_prog
  end
end

let equal_pairs equal xs ys =
  match List.fold2 ~init:true ~f:(fun acc x y -> acc && equal x y) xs ys with
  | List.Or_unequal_lengths.Ok b -> b
  | List.Or_unequal_lengths.Unequal_lengths -> false
;;
