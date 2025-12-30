open! Base

type 'mono t
type 'mono heart

val fresh_heart : (module Monotonic.S with type t = 'mono) -> 'mono heart
val empty : 'mono heart -> 'mono t
val declare : 'mono t -> heart:'mono heart -> str:string -> 'mono t
val fetch : 'mono t -> str:string -> 'mono Option.t
val declare_and_fetch : 'mono t -> heart:'mono heart -> str:string -> 'mono * 'mono t
val merge : 'mono t -> 'mono t -> 'mono t
