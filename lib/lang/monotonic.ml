open! Base

module type S = sig
  type t

  val to_string : t -> string
  val sexp_of_t : t -> Sexp.t
  val zero : t
  val succ : t -> t

  include Comparable.S with type t := t
end

module Make (Item : sig
    val prefix : string
  end) : S = struct
  module T = struct
    type t = int [@@deriving compare]

    let to_string m = Item.prefix ^ Int.to_string m
    let sexp_of_t m = Sexp.List [ Sexp.Atom Item.prefix; sexp_of_int m ]
    let zero = 0
    let succ x = x + 1
  end

  include T
  include Comparable.Make (T)
end
