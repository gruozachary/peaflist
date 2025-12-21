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

(* A module that contains parsers for identifiers *)
module Ident : sig
  (* A parser for an identifier that starts with lowercase *)
  val lower : string Peasec.t

  (* A parser for an identifier that starts with uppercase *)
  val upper : string Peasec.t

  (* A parser for an identifier that starts with a dash (') *)
  val dash : string Peasec.t

  (* A parser for an identifier for a type *)
  val ty : string Peasec.t

  (* A parser for a identifier character *)
  val char : char Peasec.t
end

val expr : unit -> Lang.Ast.Expr.t Peasec.t
val val_decl : Lang.Ast.Decl.t Peasec.t
(* A peasec parser that parses a val decl *)

val type_decl : Lang.Ast.Decl.t Peasec.t
(* A peasec parser that parses a type decl*)

val prog : Lang.Ast.Decl.t list Peasec.t
(* A peasec parser that parses a Peaflist program  *)
