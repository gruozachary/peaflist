open! Base

type t

module Merge_element : sig
  include module type of Map.Merge_element

  type nonrec t = (Scheme.t, Scheme.t) t
end

val empty : unit -> t
val introduce : t -> id:string -> sc:Scheme.t -> t
val lookup : t -> id:string -> Scheme.t Option.t
val map : t -> f:(Scheme.t -> Scheme.t) -> t
val free_tvars : t -> (Tvar.t, Tvar.comparator_witness) Set.t
val merge : t -> t -> f:(key:string -> Merge_element.t -> Scheme.t Option.t) -> t
val size : t -> int
