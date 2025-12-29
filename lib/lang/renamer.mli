open! Base

type t
type heart

val fresh_heart : unit -> heart
val empty : t
val declare : t -> heart:heart -> str:string -> t
val fetch : t -> str:string -> Ident.t Option.t
