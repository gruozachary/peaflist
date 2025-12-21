type t

val create : unit -> t
val fresh_tv : t -> Tvar.t
val fresh : t -> Tau.t
