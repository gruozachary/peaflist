open! Base

type t =
  | TInt
  | TVar of Type_var.t
  | TFun of t * t
  | TProd of t list
  | TCon of string * t list

val to_string : t -> string
val free_tvars : t -> (Type_var.t, Type_var.comparator_witness) Set.t
val occurs : Type_var.t -> t -> bool
