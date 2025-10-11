open! Base
open! Sqc.Peasec
open! Let_syntax

let id =
  let%map first = letter
  and rest = many (first_ok (first_ok letter digit) (char '_')) in
  String.of_char_list (first :: rest)

include Sqc.Peasec
include Sqc.Peasec.Let_syntax
