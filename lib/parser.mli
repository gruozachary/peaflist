(* Peaflist v1 grammar *)
(*
  Prog      ::= (ValDecl | TypeDecl)* ;

  ValDecl   ::= "vd" Id ":=" Expr ;
  TypeDecl  ::= "td" "(" ( TVar ( "," TVar )* )? ")" Id ":=" ( "|" Id Type? )+ ;

  Type      ::= ProdType ( "->" ProdType)* ;
  ProdType  ::= AtomType ( "*" AtomType )* ;
  AtomType  ::= TId | "(" Type ")" ;

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
  TVar      ::= "'" Id
  TId       ::= Id | TVar
  Letter    ::= "a" | ... "z" | "A" | ... | "Z" ;
*)

val prog : Ast.decl list Peasec.t
(* A peasec parser that parses a Peaflist program  *)
