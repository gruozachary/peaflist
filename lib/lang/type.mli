open! Base

type t =
  | TVar of Type_var.t
  | TFun of t * t
  | TProd of t list
  | TCon of Type_ident.t * t list

val to_string : t -> string
val sexp_of_t : t -> Sexp.t
val free_tvars : t -> (Type_var.t, Type_var.comparator_witness) Set.t
val occurs : Type_var.t -> t -> bool
