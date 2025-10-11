open! Base
open! Sqc.Peasec
open! Let_syntax

let char c = satisfy (equal_char c)
let letter = satisfy Char.is_alpha
let digit = satisfy Char.is_digit
let space = satisfy Char.is_whitespace
let spaces = ignore_m (many space)

let symbol p =
  let%bind _ = spaces in
  p

let id =
  let%map first = letter
  and rest = many (first_ok (first_ok letter digit) (char '_')) in
  String.of_char_list (first :: rest)

include Sqc.Peasec
include Sqc.Peasec.Let_syntax
