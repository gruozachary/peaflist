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

let keywords = Set.of_list (module String) [ "fun"; "let"; "in" ]

let id : AST.id t =
  lexeme
    (let%bind first = letter in
     let%bind rest = many (first_ok (first_ok letter digit) (char '_')) in
     let x = String.of_char_list (first :: rest) in
     if Set.mem keywords x then fail else return x)

(* TODO: this will result on a horrible disaster if there are too many digits *)
and int : AST.int t =
  lexeme
    (let%bind first = digit in
     let%map rest = many digit in
     Int.of_string (String.of_char_list (first :: rest)))

let rec expr () = choice [ binding (); lambda (); append () ]

and binding () =
  let%bind _ = string "let" in
  let%bind _ = spaces_1 in
  let%bind x = id in
  let%bind _ = symbol "=" in
  let%bind e1 = expr () in
  let%bind _ = string "in" in
  let%bind _ = spaces_1 in
  let%map e2 = expr () in
  AST.Binding (x, e1, e2)

and lambda () =
  let%bind _ = string "fun" in
  let%bind _ = spaces_1 in
  let%bind x = id in
  let%bind _ = symbol "->" in
  let%map e = expr () in
  AST.Lambda (x, e)

(* TODO: make this chain_right_1 *)
and append () =
  chain_left_1 (add ())
    (let%map _ = atomic (symbol "++") in
     fun l r -> AST.BinOp (l, AST.Append, r))

and add () =
  chain_left_1 (mul ())
    (let%map x = first_ok (symbol "+") (symbol "-") in
     fun l r ->
       match x with
       | "+" -> AST.BinOp (l, AST.Plus, r)
       | "-" -> AST.BinOp (l, AST.Sub, r)
       | _ -> assert false)

and mul () =
  chain_left_1 (apply ())
    (let%map x = first_ok (symbol "*") (symbol "/") in
     fun l r ->
       match x with
       | "*" -> AST.BinOp (l, AST.Mul, r)
       | "/" -> AST.BinOp (l, AST.Div, r)
       | _ -> assert false)

and apply () =
  let%bind x = atom () in
  let%map xs = many (atom ()) in
  List.fold ~init:x ~f:(fun f x -> AST.Apply (f, x)) xs

and atom () =
  choice
    [
      (let%map x = int in
       AST.Int x);
      (let%map x = id in
       AST.Id x);
      (let%bind _ = symbol "(" in
       let%bind e = expr () in
       let%map _ = symbol ")" in
       AST.Group e);
      (let%bind _ = symbol "[" in
       let%bind es = sep_by_1 ~sep:(symbol ",") (expr ()) in
       let%map _ = symbol "]" in
       AST.List es);
    ]

include Sqc.Peasec
include Sqc.Peasec.Let_syntax
