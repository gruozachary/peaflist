open! Base

module type S = sig
  type t

  val create : Monotonic.t -> String.t -> t
  val to_string : t -> string
  val sexp_of_t : t -> Sexp.t

  include Comparable.S with type t := t
end

module type Make = functor
    (_ : sig
       val name : string
     end)
    -> S
