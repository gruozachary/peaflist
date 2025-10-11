open! Base
open! Sqc.Peasec
open! Let_syntax

(* Peaflist v1 grammar *)
(*
  Prog      ::= Expr ;

  Expr      ::= Int
              | Id
              | Expr Expr                       function application
              | "(" Expr ")"                    group
              | "fun" Id "->" Expr              lamda
              | "let" Id "=" Expr "in" Expr     binding
              | "[" ListElems? "]"
              | Expr BinOp Expr
              ;

  ListElems ::= Expr ("," Expr)* ;

  BinOp     ::= "+" | "-" | "*" | "/" | "++" ;

  Int       ::= Digit+ ;
  Digit     ::= "0" | ... | "9" ;

  Id        ::= Letter (Letter | Digit | "_")* ;
  Letter    ::= "a" | ... "z" | "A" | ... | "Z" ;
*)

let id =
  let%map first = letter
  and rest = many (first_ok (first_ok letter digit) (char '_')) in
  String.of_char_list (first :: rest)

include Sqc.Peasec
include Sqc.Peasec.Let_syntax
