open! Base

type nonrec int = int
type id = string [@@deriving eq]
type ty_id = string [@@deriving eq]
type ty_var = string [@@deriving eq]

module Pat = struct
  type t =
    | Int of int
    | Ident of id
    | Tuple of t list
    | CtorApp of id * t Option.t
  [@@deriving eq]
end

module Expr = struct
  module Bin_op = struct
    type t =
      | Plus
      | Sub
      | Mul
      | Div
    [@@deriving eq]
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
  [@@deriving eq]
end

module Ty = struct
  type t =
    | Id of id
    | App of id * t list
    | Prod of t list
    | Fun of t * t
  [@@deriving eq]
end

type decl =
  | ValDecl of id * Expr.t
  | TypeDecl of id * ty_var list * (id * Ty.t option) list
[@@deriving eq]

type prog = decl list [@@deriving eq]
