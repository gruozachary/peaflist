open! Base

module T = struct
  type t = int [@@deriving compare]

  let sexp_of_t ident = Sexp.List [ Sexp.Atom "var"; sexp_of_int ident ]
  let zero = 0
  let succ x = x + 1
end

include T
include Comparable.Make (T)
