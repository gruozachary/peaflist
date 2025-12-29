open! Base

module T = struct
  type t = int [@@deriving compare, sexp_of]

  let zero = 0
  let succ x = x + 1
end

include T
include Comparable.Make (T)
