open! Base

module Pat : sig
  type t =
    | Int of int
    | Ident of Ident.t * Type.t
    | Tuple of t list * Type.t
    | CtorApp of Ident.t * t Option.t * Type.t
end

module Expr : sig
  type t =
    | Int of int
    | Id of Ident.t * Type.t
    | Constr of Ident.t * Type.t
    | Apply of t * t * Type.t
    | Lambda of Ident.t * Type.t * t
    | Let of Ident.t * Scheme.t * t * t
    | Match of t * (Pat.t * t) list * Type.t
    | Tuple of t list * Type.t

  val zonk : Subst.t -> t -> t
  val sexp_of_t : t -> Sexp.t
end

module Decl : sig
  type t =
    | ValDecl of Ident.t * Expr.t
    | TypeDecl of Type_ident.t

  val zonk : Subst.t -> t -> t
  val sexp_of_t : t -> Sexp.t
end

module Prog : sig
  type t = Decl.t list

  val zonk : Subst.t -> t -> t
  val sexp_of_t : t -> Sexp.t
end
