open! Base

module T = struct
  type t = int [@@deriving compare, sexp_of]

  let zero = 0
  let succ x = x + 1
  let to_string = Int.to_string
end

include T
include Comparable.Make (T)
