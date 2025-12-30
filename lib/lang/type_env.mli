type t
type arity = int

val empty : t
val introduce : t -> id:Type_ident.t -> arity:arity -> t
val lookup : t -> id:Type_ident.t -> arity Option.t
