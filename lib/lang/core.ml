open! Base

module Pat = struct
  type t =
    | Int of int
    | Ident of Ident.t * Type.t
    | Tuple of t list * Type.t
    | CtorApp of Ident.t * t Option.t * Type.t
end

module Expr = struct
  type t =
    | Int of int
    | Id of Ident.t * Type.t
    | Constr of Ident.t * Type.t
    | Apply of t * t * Type.t
    | Lambda of Ident.t * Type.t * t
    | Let of Ident.t * Scheme.t * t * t
    | Match of t * (Pat.t * t) list * Type.t
    | Tuple of t list * Type.t
end

module Decl = struct
  type t =
    | ValDecl of Ident.t * Expr.t
    | TypeDecl of Ident.t
end

module Prog = struct
  type t = Decl.t list
end
