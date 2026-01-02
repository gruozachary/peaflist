open! Base

type t = int [@@deriving compare, sexp_of]

let zero = 0
let succ x = x + 1
let to_string = Int.to_string
