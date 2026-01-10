open! Base
open! Lang.Parsed_ast

type program_bundle =
  { str : string
  ; ast : Prog.t
  }

let empty = { str = ""; ast = Prog.Decls ([], ()) }

let val_declaration =
  { str = "vd double := fun x -> x * x"
  ; ast =
      Prog.Decls
        ( [ Decl.Val
              ( "double"
              , Expr.Lambda
                  ( "x"
                  , Expr.BinOp (Expr.Ident ("x", ()), `Mul, Expr.Ident ("x", ()), ())
                  , () )
              , () )
          ]
        , () )
  }
;;

let val_declaration_crazy_whitespace =
  { str = "     vd     double:=          fun                   x->x*         x"
  ; ast =
      Prog.Decls
        ( [ Decl.Val
              ( "double"
              , Expr.Lambda
                  ( "x"
                  , Expr.BinOp (Expr.Ident ("x", ()), `Mul, Expr.Ident ("x", ()), ())
                  , () )
              , () )
          ]
        , () )
  }
;;

let maybe_declaration =
  { str =
      {|
td ('a) maybe :=
  | Just of 'a
  | Nothing
        |}
  ; ast =
      Prog.Decls
        ( [ Decl.Type
              ( "maybe"
              , [ "'a" ]
              , [ "Just", Some (Ty.Var ("'a", ())); "Nothing", None ]
              , () )
          ]
        , () )
  }
;;

let list_length =
  { str =
      {|
td ('a) list :=
  | Cons of 'a * 'a list
  | Nil

vd length := fun l ->
  match l with
    | Nil         -> 0
    | Cons (x, xs) -> 1 + length xs
|}
  ; ast =
      Prog.Decls
        ( [ Decl.Type
              ( "list"
              , [ "'a" ]
              , [ ( "Cons"
                  , Some
                      (Ty.Prod
                         ( [ Ty.Var ("'a", ())
                           ; Ty.Con ("list", [ Ty.Var ("'a", ()) ], ())
                           ]
                         , () )) )
                ; "Nil", None
                ]
              , () )
          ; Decl.Val
              ( "length"
              , Expr.Lambda
                  ( "l"
                  , Expr.Match
                      ( Expr.Ident ("l", ())
                      , [ Pat.Constr ("Nil", [], ()), Expr.Int (0, ())
                        ; ( Pat.Constr
                              ("Cons", [ Pat.Ident ("x", ()); Pat.Ident ("xs", ()) ], ())
                          , Expr.BinOp
                              ( Expr.Int (1, ())
                              , `Plus
                              , Expr.Apply
                                  (Expr.Ident ("length", ()), Expr.Ident ("xs", ()), ())
                              , () ) )
                        ]
                      , () )
                  , () )
              , () )
          ]
        , () )
  }
;;

(*
   Sqc.Peasec.exec Sqc.Parser.prog "
td ('a, 'b) test :=
  | A of ('a, 'b, int) tri option * char -> lol * int * long chain types -> ('a, 'b) result
";;
*)
let nasty_type =
  { str =
      {|
td ('a, 'b) test :=
  | A of ('a, 'b, int) tri option * char -> lol * int * long chain types -> ('a, 'b) result
|}
  ; ast =
      Prog.Decls
        ( [ Decl.Type
              ( "test"
              , [ "'a"; "'b" ]
              , [ ( "A"
                  , Some
                      (Ty.Fun
                         ( Ty.Prod
                             ( [ Ty.Con
                                   ( "option"
                                   , [ Ty.Con
                                         ( "tri"
                                         , [ Ty.Var ("'a", ())
                                           ; Ty.Var ("'b", ())
                                           ; Ty.Con ("int", [], ())
                                           ]
                                         , () )
                                     ]
                                   , () )
                               ; Ty.Con ("char", [], ())
                               ]
                             , () )
                         , Ty.Fun
                             ( Ty.Prod
                                 ( [ Ty.Con ("lol", [], ())
                                   ; Ty.Con ("int", [], ())
                                   ; Ty.Con
                                       ( "types"
                                       , [ Ty.Con
                                             ("chain", [ Ty.Con ("long", [], ()) ], ())
                                         ]
                                       , () )
                                   ]
                                 , () )
                             , Ty.Con
                                 ("result", [ Ty.Var ("'a", ()); Ty.Var ("'b", ()) ], ())
                             , () )
                         , () )) )
                ]
              , () )
          ]
        , () )
  }
;;
