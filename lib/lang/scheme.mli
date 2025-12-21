open! Base

type t = Forall of Tvar.t list * Tau.t

val to_string : t -> string
val free_tvars : t -> (Tvar.t, Tvar.comparator_witness) Set.t
val of_tau : Tau.t -> t
