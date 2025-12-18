module Value : sig
  type t

  val to_string : t -> string
end

module Env : sig
  type t

  val lookup : t -> id:string -> Value.t
  val empty : t
end

val eval : env:Env.t -> Ast.Expr.t -> (Value.t, string) Result.t
