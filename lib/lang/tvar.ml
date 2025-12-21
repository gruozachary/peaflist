open! Base

module T = struct
  type t = int [@@deriving compare, sexp]

  let to_string tvar = "__tvar" ^ Int.to_string tvar
  let of_int x = x
end

include T
include Comparable.Make (T)
