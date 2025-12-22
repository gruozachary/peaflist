type t

val create : unit -> t
val fresh_tv : t -> Type_var.t
val fresh : t -> Type.t
