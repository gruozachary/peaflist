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

let id : AST.id t =
  lexeme
    (let%map first = letter
     and rest = many (first_ok (first_ok letter digit) (char '_')) in
     String.of_char_list (first :: rest))

(* TODO: this will result on a horrible disaster if there are too many digits *)
and int : AST.int t =
  lexeme
    (let%map first = digit and rest = many digit in
     Int.of_string (String.of_char_list (first :: rest)))

let bin_op : AST.bin_op t =
  match%map
    choice [ symbol "+"; symbol "-"; symbol "*"; symbol "/"; symbol "++" ]
  with
  | "+" -> AST.Plus
  | "-" -> AST.Sub
  | "*" -> AST.Mul
  | "/" -> AST.Div
  | "++" -> AST.Append
  | _ -> assert false

let expr : AST.expr t =
  fix (fun expr ->
      choice
        [
          (* (let%map e1 = expr and e2 = expr in
           AST.Apply (e1, e2)); *)
          (let%map _ = symbol "(" and e = expr and _ = symbol ")" in
           AST.Group e);
          (let%map _ = symbol "fun"
           and x = id
           and _ = symbol "->"
           and e = expr in
           AST.Lambda (x, e));
          (let%map _ = symbol "let"
           and x = id
           and _ = symbol "="
           and e1 = expr
           and _ = symbol "in"
           and e2 = expr in
           AST.Binding (x, e1, e2));
          (let%map _ = symbol "["
           and es = sep_by_1 ~sep:(symbol ",") expr
           and _ = symbol "]" in
           AST.List es);
          (let%map x = id in
           AST.Id x);
          (let%map x = int in
           AST.Int x);
          (* (let%map e1 = expr and o = bin_op and e2 = expr in
           AST.BinOp (e1, o, e2)); *)
        ])

include Sqc.Peasec
include Sqc.Peasec.Let_syntax
