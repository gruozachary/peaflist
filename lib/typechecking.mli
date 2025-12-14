module Tau : sig
  type t

  val to_string : t -> string
end

module Scheme : sig
  type t

  val to_string : t -> string
end

module Gamma : sig
  type t

  val lookup : t -> id:string -> Scheme.t Option.t
  val empty : unit -> t
end

module TyEnv : sig
  type t
  type arity = int

  val lookup : t -> id:string -> arity Option.t
end

module Ctx : sig
  type t

  val empty : unit -> t

  module Env : sig
    val get : t -> Gamma.t
  end

  module Tenv : sig
    val get : t -> TyEnv.t
  end
end

val typecheck_expr : Ctx.t -> Ast.Expr.t -> (Tau.t, string) Result.t
val typecheck_val_decl : Ctx.t -> string -> Ast.Expr.t -> (Ctx.t, string) Result.t

val typecheck_type_decl
  :  Ctx.t
  -> string
  -> string list
  -> (string * Ast.Ty.t Option.t) list
  -> (Ctx.t, string) Result.t

val typecheck_prog : Ctx.t -> Ast.decl list -> (Ctx.t, string) Result.t
