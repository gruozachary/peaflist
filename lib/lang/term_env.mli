open! Base

type t

module Merge_element : sig
  include module type of Map.Merge_element

  type nonrec t = (Scheme.t, Scheme.t) t
end

val empty : unit -> t
val introduce : t -> id:Ident.t -> sc:Scheme.t -> t
val lookup : t -> id:Ident.t -> Scheme.t Option.t
val map : t -> f:(Scheme.t -> Scheme.t) -> t
val free_tvars : t -> (Type_var.t, Type_var.comparator_witness) Set.t
val merge : t -> t -> f:(key:Ident.t -> Merge_element.t -> Scheme.t Option.t) -> t
val size : t -> int
