open! Base

type t
type heart

val fresh_heart : unit -> heart
val empty : heart -> t
val declare : t -> heart:heart -> str:string -> t
val fetch : t -> str:string -> Ident.t Option.t
val declare_and_fetch : t -> heart:heart -> str:string -> Ident.t * t
val merge : t -> t -> t
