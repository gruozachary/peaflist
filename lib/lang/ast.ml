module Pat = struct
  type ('vident, 'cident) t' =
    | Int of int
    | Ident of 'vident
    | Tuple of ('vident, 'cident) t List.t
    | Constr of 'cident * ('vident, 'cident) t List.t

  and ('vident, 'cident) t = { node : ('vident, 'cident) t' }
end

module Expr = struct
  module Bin_op = struct
    type t =
      | Plus
      | Sub
      | Mul
      | Div
  end

  type ('vident, 'cident) t' =
    | Int of int
    | Ident of 'vident
    | Constr of 'cident * ('vident, 'cident) t List.t
    | Apply of ('vident, 'cident) t * ('vident, 'cident) t
    | Group of ('vident, 'cident) t
    | Lambda of 'vident * ('vident, 'cident) t
    | Let of 'vident * ('vident, 'cident) t * ('vident, 'cident) t
    | Match of
        ('vident, 'cident) t * (('vident, 'cident) Pat.t * ('vident, 'cident) t) List.t
    | Tuple of ('vident, 'cident) t List.t
    | BinOp of ('vident, 'cident) t * Bin_op.t * ('vident, 'cident) t

  and ('vident, 'cident) t = { node : ('vident, 'cident) t' }
end

module Ty = struct
  type ('tvar, 'tident) t' =
    | Var of 'tvar
    | Con of 'tident * ('tvar, 'tident) t List.t
    | Prod of ('tvar, 'tident) t List.t
    | Fun of ('tvar, 'tident) t * ('tvar, 'tident) t

  and ('tvar, 'tident) t = { node : ('tvar, 'tident) t' }
end

module Decl = struct
  type ('vident, 'cident, 'tvar, 'tident) t' =
    | Val of 'vident * ('vident, 'cident) Expr.t
    | Type of 'tident * 'tvar List.t * ('cident * ('tvar, 'tident) Ty.t Option.t) List.t

  and ('vident, 'cident, 'tvar, 'tident) t =
    { node : ('vident, 'cident, 'tvar, 'tident) t' }
end

module Prog = struct
  type ('vident, 'cident, 'tvar, 'tident) t' =
    ('vident, 'cident, 'tvar, 'tident) Decl.t List.t
end
