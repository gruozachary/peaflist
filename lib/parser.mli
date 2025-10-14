(* Peaflist v1 grammar *)
(*
  Prog      ::= (ValDecl | TypeDecl)* ;

  ValDecl   ::= "vd" Id ":=" Expr ;
  TypeDecl  ::= "td" Id ":=" ( "|" Id Type? )+ ;

  Type      ::= ProdType ( "->" ProdType)* ;
  ProdType  ::= AtomType ( "*" AtomType )* ;
  AtomType  ::= Id | "(" Type ")" ;

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
    | List of expr list
    | Tuple of expr list
    | BinOp of expr * bin_op * expr

  type ty = TyId of id | TyProd of ty * ty | TyFun of ty * ty
  type decl = ValDecl of id * expr | TypeDecl of id * (id * ty option) list
  type prog = decl list
end

val prog : AST.decl list Peasec.t
(* A peasec parser that parses a Peaflist program  *)
