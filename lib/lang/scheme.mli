open! Base

type t = Forall of Tvar.t list * Type.t

val to_string : t -> string
val free_tvars : t -> (Tvar.t, Tvar.comparator_witness) Set.t
val of_type : Type.t -> t
