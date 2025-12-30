open! Base

module T = struct
  type t = int [@@deriving compare, sexp]

  let to_string tvar = "__tvar" ^ Int.to_string tvar
  let sexp_of_t ident = Sexp.List [ Sexp.Atom "tvar"; sexp_of_int ident ]
  let zero = 0
  let succ x = x + 1
end

include T
include Comparable.Make (T)
