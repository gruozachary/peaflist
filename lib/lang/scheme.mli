open! Base

type t = Forall of Type_var.t list * Type.t

val to_string : t -> string
val sexp_of_t : t -> Sexp.t
val free_tvars : t -> (Type_var.t, Type_var.comparator_witness) Set.t
val of_type : Type.t -> t
