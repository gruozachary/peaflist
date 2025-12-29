open! Base

type t

val empty : unit -> t
val declare : t -> str:string -> t
val fetch : t -> str:string -> Ident.t Option.t
