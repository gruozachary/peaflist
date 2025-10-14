(* Peaflist v1 grammar *)
(*
  Prog      ::= ValDecl* ;

  ValDecl   ::= "vd" Id ":=" Expr ;
  TypeDecl  ::= "td" Id ":=" ( "|" Id Type? )+

  Type      ::= Id | "(" Type ")" | Type ( "*" Type )*

  Expr      ::= Int
              | Id
              | Expr Expr                       function application
              | "(" Expr ")"                    group
              | "fun" Id "->" Expr              lamda
              | "let" Id "=" Expr "in" Expr     binding
              | "{" ( Expr ( "," Expr)* )? "}"  tuple
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

module AST : sig
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

  type type_ = type_atom list
  and type_atom = Tid of id | Type of type_

  type decl = ValDecl of id * expr | TypeDecl of id * (id * type_ option) list
  type prog = decl list
end

val prog : AST.decl list Peasec.t
(* A peasec parser that parses a Peaflist program  *)
