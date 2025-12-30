open! Base

type t

val to_string : t -> string
val sexp_of_t : t -> Sexp.t
val zero : t
val succ : t -> t

include Comparable.S with type t := t
