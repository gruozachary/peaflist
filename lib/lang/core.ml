open! Base

module Pat = struct
  type t =
    | Int of int
    | Ident of Ident.t * Type.t
    | Tuple of t list * Type.t
    | CtorApp of Ident.t * t Option.t * Type.t
  [@@deriving sexp_of]

  let rec zonk sub = function
    | Int x -> Int x
    | Ident (ident, t) -> Ident (ident, Subst.zonk_type ~sub t)
    | Tuple (ts, t) -> Tuple (List.map ~f:(zonk sub) ts, Subst.zonk_type ~sub t)
    | CtorApp (ident, p_opt, t) ->
      CtorApp (ident, Option.map ~f:(zonk sub) p_opt, Subst.zonk_type ~sub t)
  ;;
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
  [@@deriving sexp_of]

  let rec zonk sub = function
    | Int x -> Int x
    | Id (ident, t) -> Id (ident, Subst.zonk_type ~sub t)
    | Constr (ident, t) -> Constr (ident, Subst.zonk_type ~sub t)
    | Apply (e, e', t) -> Apply (zonk sub e, zonk sub e', Subst.zonk_type ~sub t)
    | Lambda (ident, t, e) -> Lambda (ident, Subst.zonk_type ~sub t, zonk sub e)
    | Let (ident, scheme, e, e') ->
      Let (ident, scheme, zonk sub e, zonk sub e')
      (* note to self: scheme is generalised at creation and instantiated at use, meaning you do not need to zonk it*)
    | Match (e, ps, t) ->
      Match
        ( zonk sub e
        , List.map ~f:(fun (p, t) -> Pat.zonk sub p, zonk sub t) ps
        , Subst.zonk_type ~sub t )
    | Tuple (es, t) -> Tuple (List.map ~f:(zonk sub) es, Subst.zonk_type ~sub t)
  ;;
end

module Decl = struct
  type t =
    | ValDecl of Ident.t * Expr.t
    | TypeDecl of string
  [@@deriving sexp_of]

  let zonk sub = function
    | ValDecl (ident, e) -> ValDecl (ident, Expr.zonk sub e)
    | TypeDecl str -> TypeDecl str
  ;;
end

module Prog = struct
  type t = Decl.t list [@@deriving sexp_of]

  let zonk sub = List.map ~f:(Decl.zonk sub)
end
