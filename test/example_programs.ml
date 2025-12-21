open! Base
open! Lang.Ast

type program_bundle =
  { str : string
  ; ast : Prog.t
  }

let empty = { str = ""; ast = [] }

let val_declaration =
  { str = "vd double := fun x -> x * x"
  ; ast =
      [ Decl.ValDecl
          ( "double"
          , Expr.Lambda ("x", Expr.BinOp (Expr.Id "x", Expr.Bin_op.Mul, Expr.Id "x")) )
      ]
  }
;;

let val_declaration_crazy_whitespace =
  { str = "     vd     double:=          fun                   x->x*         x"
  ; ast =
      [ Decl.ValDecl
          ( "double"
          , Expr.Lambda ("x", Expr.BinOp (Expr.Id "x", Expr.Bin_op.Mul, Expr.Id "x")) )
      ]
  }
;;

let maybe_declaration =
  { str =
      {|
td ('a) maybe :=
  | Just of 'a
  | Nothing
        |}
  ; ast = [ TypeDecl ("maybe", [ "'a" ], [ "Just", Some (Ty.Id "'a"); "Nothing", None ]) ]
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
      [ TypeDecl
          ( "list"
          , [ "'a" ]
          , [ "Cons", Some (Ty.Prod [ Ty.Id "'a"; Ty.App ("list", [ Ty.Id "'a" ]) ])
            ; "Nil", None
            ] )
      ; Decl.ValDecl
          ( "length"
          , Expr.Lambda
              ( "l"
              , Expr.Match
                  ( Expr.Id "l"
                  , [ Pat.CtorApp ("Nil", None), Expr.Int 0
                    ; ( Pat.CtorApp
                          ( "Cons"
                          , Option.Some (Pat.Tuple [ Pat.Ident "x"; Pat.Ident "xs" ]) )
                      , Expr.BinOp
                          ( Expr.Int 1
                          , Expr.Bin_op.Plus
                          , Expr.Apply (Expr.Id "length", Expr.Id "xs") ) )
                    ] ) ) )
      ]
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
      [ TypeDecl
          ( "test"
          , [ "'a"; "'b" ]
          , [ ( "A"
              , Some
                  (Ty.Fun
                     ( Ty.Prod
                         [ Ty.App
                             ( "option"
                             , [ Ty.App ("tri", [ Ty.Id "'a"; Ty.Id "'b"; Ty.Id "int" ]) ]
                             )
                         ; Ty.Id "char"
                         ]
                     , Ty.Fun
                         ( Ty.Prod
                             [ Ty.Id "lol"
                             ; Ty.Id "int"
                             ; Ty.App ("types", [ Ty.App ("chain", [ Ty.Id "long" ]) ])
                             ]
                         , Ty.App ("result", [ Ty.Id "'a"; Ty.Id "'b" ]) ) )) )
            ] )
      ]
  }
;;
