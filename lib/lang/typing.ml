open! Base

module Make_var () : sig
  type t

  val zero : t
  val succ : t -> t

  include Comparable.S with type t := t
end = struct
  module T = struct
    type t = int [@@deriving compare, sexp_of]

    let zero = 0
    let succ x = x + 1
  end

  include T
  include Comparable.Make (T)
end

module Gen_var = Make_var ()
module Uni_var = Make_var ()

module Type = struct
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
end

module Scheme = struct
  type _ t = Forall : Gen_var.t list * 'any Type.t -> 'any t
  type any_t = Any : 'any t -> any_t
  type unified_t = Type.phantom_unified t
  type ununified_t = Type.phantom_ununified t
end
