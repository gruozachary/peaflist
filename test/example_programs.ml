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
