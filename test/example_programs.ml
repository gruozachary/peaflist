open! Base
open! Sqc

type program_bundle = { str : string; ast : Ast.prog }

let empty = { str = ""; ast = [] }

let val_declaration =
  {
    str = "vd double := fun x -> x * x";
    ast =
      [
        Ast.ValDecl
          ( "double",
            Ast.Lambda ("x", Ast.BinOp (Ast.Id "x", Ast.Mul, Ast.Id "x")) );
      ];
  }

let val_declaration_crazy_whitespace =
  {
    str = "     vd     double:=          fun                   x->x*         x";
    ast =
      [
        Ast.ValDecl
          ( "double",
            Ast.Lambda ("x", Ast.BinOp (Ast.Id "x", Ast.Mul, Ast.Id "x")) );
      ];
  }

let maybe_declaration =
  {
    str = {|
td ('a) maybe :=
  | Just of 'a
  | Nothing
        |};
    ast =
      [
        Ast.TypeDecl
          ( "maybe",
            [ "'a" ],
            [ ("Just", Some (Ast.TyId "'a")); ("Nothing", None) ] );
      ];
  }

let list_length =
  {
    str =
      {|
td ('a) list :=
  | Cons of 'a * 'a list
  | Nil

vd length := fun l ->
  match l with
    | Nil       -> 0
    | Cons x xs -> 1 + length xs
|};
    ast =
      [
        Ast.TypeDecl
          ( "list",
            [ "'a" ],
            [
              ( "Cons",
                Some
                  (Ast.TyProd
                     (Ast.TyId "'a", Ast.TyApp (Ast.TyId "'a", Ast.TyId "list")))
              );
              ("Nil", None);
            ] );
        Ast.ValDecl
          ( "length",
            Ast.Lambda
              ( "l",
                Ast.Match
                  ( Ast.Id "l",
                    [
                      (Ast.Id "Nil", Ast.Int 0);
                      ( Ast.Apply
                          (Ast.Apply (Ast.Id "Cons", Ast.Id "x"), Ast.Id "xs"),
                        Ast.BinOp
                          ( Ast.Int 1,
                            Ast.Plus,
                            Ast.Apply (Ast.Id "length", Ast.Id "xs") ) );
                    ] ) ) );
      ];
  }
