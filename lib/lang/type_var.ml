open! Base

module T = struct
  type t = int [@@deriving compare, sexp]

  let to_string tvar = "__tvar" ^ Int.to_string tvar
  let zero = 0
  let succ x = x + 1
end

include T
include Comparable.Make (T)
