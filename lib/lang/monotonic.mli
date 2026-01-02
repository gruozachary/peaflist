open! Base

type t

val zero : t
val succ : t -> t
val to_string : t -> string
val sexp_of_t : t -> Sexp.t

include Comparable.S with type t := t
