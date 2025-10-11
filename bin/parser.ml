open! Base
open! Sqc.Peasec
open! Let_syntax

let char c = satisfy (equal_char c)
let letter = satisfy Char.is_alpha
let digit = satisfy Char.is_digit
let space = satisfy Char.is_whitespace
let spaces = ignore_m (many space)

let string s =
  let rec string_check i =
    if i < String.length s then
      let%bind _ = char s.[i] in
      string_check (i + 1)
    else return s
  in
  string_check 0

let lexeme p =
  let%map x = p and _ = spaces in
  x

let fully p =
  let%map _ = spaces and x = p and _ = eof in
  x

let symbol s = lexeme (string s)

let id =
  let%map first = letter
  and rest = many (first_ok (first_ok letter digit) (char '_')) in
  String.of_char_list (first :: rest)

include Sqc.Peasec
include Sqc.Peasec.Let_syntax
