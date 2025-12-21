type t
type arity = int

val empty : t
val introduce : t -> id:string -> arity:arity -> t
val lookup : t -> id:string -> arity Option.t
