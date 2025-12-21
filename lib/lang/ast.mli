type id = string
type ty_id = string
type ty_var = string

module Pat : sig
  type t =
    | Int of int
    | Ident of id
    | Tuple of t list
    | CtorApp of id * t Option.t
end

module Expr : sig
  module Bin_op : sig
    type t =
      | Plus
      | Sub
      | Mul
      | Div
  end

  type t =
    | Int of int
    | Id of id
    | Constr of id
    | Apply of t * t
    | Group of t
    | Lambda of id * t
    | Binding of id * t * t
    | Match of t * (Pat.t * t) list
    | Tuple of t list
    | BinOp of t * Bin_op.t * t

  val typecheck : Ctx.t -> t -> (Tau.t, string) Result.t
end

module Ty : sig
  type t =
    | Id of id
    | App of id * t list
    | Prod of t list
    | Fun of t * t
end

module Decl : sig
  type t =
    | ValDecl of id * Expr.t
    | TypeDecl of id * ty_var list * (id * Ty.t option) list

  val typecheck : Ctx.t -> t -> (Ctx.t, string) Result.t
end

module Prog : sig
  type t = Decl.t list

  val typecheck : Ctx.t -> t -> (Ctx.t, string) Result.t
end
