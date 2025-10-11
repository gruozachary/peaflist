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

module AST = struct
  type nonrec int = int
  type id = string
  type bin_op = Plus | Sub | Mul | Div | Append

  type expr =
    | Int of int
    | Id of id
    | Apply of expr * expr
    | Group of expr
    | Lambda of id * expr
    | Binding of id * expr * expr
    | List of list_elems
    | BinOp of expr * bin_op * expr

  and list_elems = expr list
end

let expr : AST.expr t = return (AST.Int 5)

and id : AST.id t =
  let%map first = letter
  and rest = many (first_ok (first_ok letter digit) (char '_')) in
  String.of_char_list (first :: rest)

(* TODO: this will result on a horrible disaster if there are too many digits *)
and int : AST.int t =
  let%map first = digit and rest = many digit in
  Int.of_string (String.of_char_list (first :: rest))

and bin_op : AST.bin_op t =
  match%map
    choice [ symbol "+"; symbol "-"; symbol "*"; symbol "/"; symbol "++" ]
  with
  | "+" -> AST.Plus
  | "-" -> AST.Sub
  | "*" -> AST.Mul
  | "/" -> AST.Div
  | "++" -> AST.Append
  | _ -> assert false

include Sqc.Peasec
include Sqc.Peasec.Let_syntax
