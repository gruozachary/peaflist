(* Peaflist v1 grammar *)
(*
  Prog      ::= (ValDecl | TypeDecl)* ;

  ValDecl   ::= "vd" Id ":=" Expr ;
  TypeDecl  ::= "td" "(" ( TVar ( "," TVar )* )? ")" Id ":=" ( "|" Id ("of" Type)? )+ ;

  Type      ::= ProdType ( "->" ProdType)* ;
  ProdType  ::= AppType ( "*" AppType )* ;
  AppType   ::= ( "(" Type ( "," Type )* ")" Tid | TId ) TId*

  Expr      ::= Int
              | LowerId                                     normal identifier
              | UpperId                                     constructor
              | Expr Expr                                   function application
              | "(" Expr ")"                                group
              | "fun" Id "->" Expr                          lamda
              | "let" Id "=" Expr "in" Expr                 binding
              | "match" Expr "with" ("|" Expr "->" Expr)+   match
              | "{" ( Expr ( "," Expr)* )? "}"              tuple
              | "[" ListElems? "]"
              | Expr BinOp Expr
              ;

  ListElems ::= Expr ("," Expr)* ;

  BinOp     ::= "+" | "-" | "*" | "/" | "++" ;

  Int       ::= Digit+ ;
  Digit     ::= "0" | ... | "9" ;

  LowerId   ::= Lowercase (Letter | Digit | "_")* ;
  UpperId   ::= Uppercase (Letter | Digit | "_")* ;
  DashId    ::= "'" Letter (Letter | Digit | "_")* ;

  TypeId    ::= LowerId | DashId

  Letter    ::= Lowercase | Uppercase ;
  Lowercase ::= "a" | ... "z" ;
  Uppercase ::= "A" | ... "Z" ;
*)

val prog : Ast.decl list Peasec.t
(* A peasec parser that parses a Peaflist program  *)
