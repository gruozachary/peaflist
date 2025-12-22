open! Base

type t

val to_string : t -> string
val of_int : int -> t

include Comparable.S with type t := t
