open! Base

type t =
  | TVar of Tvar.t
  | TFun of t * t
  | TProd of t list
  | TCon of string * t list

val to_string : t -> string
val free_tvars : t -> (Tvar.t, Tvar.comparator_witness) Set.t
val occurs : Tvar.t -> t -> bool
