open! Base

type t

val zero : t
val succ : t -> t

include Comparable.S with type t := t
