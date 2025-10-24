open! Base
open! Sqc

type program_bundle =
  { str : string
  ; ast : Ast.prog
  }

let empty = { str = ""; ast = [] }

let val_declaration =
  { str = "vd double := fun x -> x * x"
  ; ast =
      [ Ast.ValDecl
          ("double", Ast.Lambda ("x", Ast.BinOp (Ast.Id "x", Ast.Mul, Ast.Id "x")))
      ]
  }
;;

let val_declaration_crazy_whitespace =
  { str = "     vd     double:=          fun                   x->x*         x"
  ; ast =
      [ Ast.ValDecl
          ("double", Ast.Lambda ("x", Ast.BinOp (Ast.Id "x", Ast.Mul, Ast.Id "x")))
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
  ; ast =
      [ Ast.TypeDecl ("maybe", [ "'a" ], [ "Just", Some (Ast.TyId "'a"); "Nothing", None ])
      ]
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
    | Nil       -> 0
    | Cons x xs -> 1 + length xs
|}
  ; ast =
      [ Ast.TypeDecl
          ( "list"
          , [ "'a" ]
          , [ ( "Cons"
              , Some (Ast.TyProd [ Ast.TyId "'a"; Ast.TyApp ("list", [ Ast.TyId "'a" ]) ])
              )
            ; "Nil", None
            ] )
      ; Ast.ValDecl
          ( "length"
          , Ast.Lambda
              ( "l"
              , Ast.Match
                  ( Ast.Id "l"
                  , [ Ast.Id "Nil", Ast.Int 0
                    ; ( Ast.Apply (Ast.Apply (Ast.Id "Cons", Ast.Id "x"), Ast.Id "xs")
                      , Ast.BinOp
                          (Ast.Int 1, Ast.Plus, Ast.Apply (Ast.Id "length", Ast.Id "xs"))
                      )
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
      [ Ast.TypeDecl
          ( "test"
          , [ "'a"; "'b" ]
          , [ ( "A"
              , Some
                  (Ast.TyFun
                     ( Ast.TyProd
                         [ Ast.TyApp
                             ( "option"
                             , [ Ast.TyApp
                                   ( "tri"
                                   , [ Ast.TyId "'a"; Ast.TyId "'b"; Ast.TyId "int" ] )
                               ] )
                         ; Ast.TyId "char"
                         ]
                     , Ast.TyFun
                         ( Ast.TyProd
                             [ Ast.TyId "lol"
                             ; Ast.TyId "int"
                             ; Ast.TyApp
                                 ("types", [ Ast.TyApp ("chain", [ Ast.TyId "long" ]) ])
                             ]
                         , Ast.TyApp ("result", [ Ast.TyId "'a"; Ast.TyId "'b" ]) ) )) )
            ] )
      ]
  }
;;
