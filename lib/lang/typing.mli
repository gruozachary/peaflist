open! Base

module Uni_var : sig
  type t

  val zero : t
  val succ : t -> t
  val sexp_of_t : t -> Sexp.t
  val to_string : t -> string

  include Comparable.S with type t := t
end

module Gen_var : sig
  type t

  val zero : t
  val succ : t -> t
  val sexp_of_t : t -> Sexp.t
  val to_string : t -> string

  include Comparable.S with type t := t
end

module Type : sig
  type phantom_ununified
  type phantom_unified

  type _ t =
    | Gen : Gen_var.t -> 'any t
    | Uni : uni ref -> phantom_ununified t
    | Fun : 'any t * 'any t -> 'any t
    | Prod : 'any t list -> 'any t
    | Con : Type_ident.t * 'any t list -> 'any t

  and any_t = Any : 'any t -> any_t

  and uni =
    | Unbound of Uni_var.t
    | Link of phantom_ununified t

  type unified_t = phantom_unified t
  type ununified_t = phantom_ununified t

  val sexp_of_t : 'a t -> Sexplib0.Sexp.t

  val to_string
    :  ?mapping:(Gen_var.t, string, Gen_var.comparator_witness) Base.Map.t
    -> 'a t
    -> string
end

module Scheme : sig
  type _ t = Forall : Gen_var.t list * 'any Type.t -> 'any t
  type any_t = Any : 'any t -> any_t
  type unified_t = Type.phantom_unified t
  type ununified_t = Type.phantom_ununified t

  val sexp_of_t : 'a t -> Sexplib0.Sexp.t

  val to_string
    :  ?mapping:(Gen_var.t, string, Gen_var.comparator_witness) Base.Map.t
    -> 'a t
    -> string
end
