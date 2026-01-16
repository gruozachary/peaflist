open! Base

type void = |

module Make_pat (M : sig
    type var_ident
    type ctor_ident
    type for_int
    type for_ident
    type for_tuple
    type for_constr
  end) =
struct
  type t =
    | Int of int * M.for_int
    | Ident of M.var_ident * M.for_ident
    | Tuple of t list * M.for_tuple
    | Constr of M.ctor_ident * t list * M.for_constr
end

module Make_expr (M : sig
    module Pat : sig
      type t
    end

    type var_ident
    type ctor_ident
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
  end) =
struct
  module Bin_op = struct
    type t =
      [ `Plus
      | `Mul
      | `Sub
      | `Div
      ]
  end

  type t =
    | Int of int * M.for_int
    | Ident of M.var_ident * M.for_ident
    | Constr of M.ctor_ident * t List.t * M.for_constr
    | Apply of t * t * M.for_apply
    | Group of t * M.for_group
    | Lambda of M.var_ident * t * M.for_lambda
    | Let of M.var_ident * t * t * M.for_let
    | Match of t * (M.Pat.t * t) List.t * M.for_match
    | Tuple of t List.t * M.for_tuple
    | BinOp of t * Bin_op.t * t * M.for_binop
end

module Make_ty (M : sig
    type type_ident
    type tvar
    type for_var
    type for_con
    type for_prod
    type for_fun
  end) =
struct
  type t =
    | Var of M.tvar * M.for_var
    | Con of M.type_ident * t list * M.for_con
    | Prod of t list * M.for_prod
    | Fun of t * t * M.for_fun
end

module Make_decl (M : sig
    module Expr : sig
      type t
    end

    module Ty : sig
      type t
    end

    type var_ident
    type type_ident
    type tvar
    type ctor_ident
    type for_val
    type for_type
    type for_ctor
  end) =
struct
  type t =
    | Val of M.var_ident * M.Expr.t * M.for_val
    | Type of
        M.type_ident
        * M.tvar list
        * (M.ctor_ident * M.Ty.t option * M.for_ctor) list
        * M.for_type
end

module Make_prog (M : sig
    module Decl : sig
      type t
    end

    type for_decls
  end) =
struct
  type t = Decls of M.Decl.t list * M.for_decls
end

module Make (M : sig
    type var_ident
    type ctor_ident
    type type_ident
    type tvar

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
      type for_ctor
    end

    module Prog : sig
      type for_decls
    end
  end) =
struct
  module Pat = Make_pat (struct
      type var_ident = M.var_ident
      type ctor_ident = M.ctor_ident

      include M.Pat
    end)

  module Expr = Make_expr (struct
      module Pat = Pat

      type var_ident = M.var_ident
      type ctor_ident = M.ctor_ident

      include M.Expr
    end)

  module Ty = Make_ty (struct
      type type_ident = M.type_ident
      type tvar = M.tvar

      include M.Ty
    end)

  module Decl = Make_decl (struct
      module Expr = Expr
      module Ty = Ty

      type var_ident = M.var_ident
      type type_ident = M.type_ident
      type ctor_ident = M.ctor_ident
      type tvar = M.tvar

      include M.Decl
    end)

  module Prog = Make_prog (struct
      module Decl = Decl
      include M.Prog
    end)
end

let equal_pairs equal xs ys =
  match List.fold2 ~init:true ~f:(fun acc x y -> acc && equal x y) xs ys with
  | List.Or_unequal_lengths.Ok b -> b
  | List.Or_unequal_lengths.Unequal_lengths -> false
;;
