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
              | "(" ( Expr ( "," Expr)* )? ")"              tuple/group
              | "fun" Id "->" Expr                          lamda
              | "let" Id "=" Expr "in" Expr                 binding
              | "match" Expr "with" ("|" Pattern "->" Expr)+   match
              | Expr BinOp Expr
              ;

  Pattern   ::= Int                                       basic constant
              | LowerId                                   variable
              | "(" ( Pattern ( "," Pattern)* )? ")"      tuple/group
              | UpperId Pattern?                          constructor application

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

val expr : unit -> Ast.Expr.t Peasec.t
val val_decl : Ast.decl Peasec.t
(* A peasec parser that parses a val decl *)

val type_decl : Ast.decl Peasec.t
(* A peasec parser that parses a type decl*)

val prog : Ast.decl list Peasec.t
(* A peasec parser that parses a Peaflist program  *)
