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

module Type = struct
  type t =
    | Gen of Gen_var.t
    | Fun of t * t
    | Prod of t list
    | Con of Type_ident.t * t list
end

module Scheme = struct
  type t = Forall of Gen_var.t list * Type.t
end
