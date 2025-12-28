open! Base

type id = string
type ty_id = string
type ty_var = string

module Pat : sig
  type t =
    | Int of int
    | Ident of id * Type.t
    | Tuple of t list * Type.t
    | CtorApp of id * t Option.t * Type.t
end

module Expr : sig
  type t =
    | Int of int
    | Id of id * Type.t
    | Constr of id * Type.t
    | Apply of t * t * Type.t
    | Lambda of id * Type.t * t
    | Let of id * Scheme.t * t * t
    | Match of t * (Pat.t * t) list * Type.t
    | Tuple of t list * Type.t
end

module Decl : sig
  type t =
    | ValDecl of id * Expr.t
    | TypeDecl of id * ty_var list * (id * Type.t Option.t) list
end

module Prog : sig
  type t = Decl.t list
end
