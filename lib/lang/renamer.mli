open! Base

type 'ident t
type 'ident heart

val fresh_heart : (module Ident.S with type t = 'ident) -> 'ident heart
val empty : 'ident heart -> 'ident t
val declare : 'ident t -> heart:'ident heart -> str:string -> 'ident t
val fetch : 'ident t -> str:string -> 'ident Option.t
val declare_and_fetch : 'ident t -> heart:'ident heart -> str:string -> 'ident * 'ident t
val merge : 'ident t -> 'ident t -> 'ident t
