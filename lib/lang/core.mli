open! Base

module Pat : sig
  type t =
    | Int of int
    | Ident of Var_ident.t * Type.t
    | Tuple of t list * Type.t
    | CtorApp of Var_ident.t * t Option.t * Type.t
end

module Expr : sig
  type t =
    | Int of int
    | Id of Var_ident.t * Type.t
    | Constr of Constr_ident.t * t List.t * Type.t
    | Apply of t * t * Type.t
    | Lambda of Var_ident.t * Type.t * t
    | Let of Var_ident.t * Scheme.t * t * t
    | Match of t * (Pat.t * t) list * Type.t
    | Tuple of t list * Type.t

  val zonk : Subst.t -> t -> t
  val sexp_of_t : t -> Sexp.t
end

module Decl : sig
  type t =
    | ValDecl of Var_ident.t * Expr.t
    | TypeDecl of Type_ident.t

  val zonk : Subst.t -> t -> t
  val sexp_of_t : t -> Sexp.t
end

module Prog : sig
  type t = Decl.t list

  val zonk : Subst.t -> t -> t
  val sexp_of_t : t -> Sexp.t
end
