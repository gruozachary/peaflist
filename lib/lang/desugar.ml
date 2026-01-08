open! Base

type t = { renamer : Rename.t }
type void = |

let bin_op_fun = function
  | `Plus -> "+"
  | `Mul -> "*"
  | `Sub -> "-"
  | `Div -> "/"
;;

let rec desugar_pat ~pat =
  let module O = Ast.Renamed.Pat in
  let open Ast.Desugared.Pat in
  match pat with
  | O.Constr (ident, pats, ()) ->
    Constr (ident, List.map ~f:(fun pat -> desugar_pat ~pat) pats, ())
  | O.Ident (ident, ()) -> Ident (ident, ())
  | O.Int (x, ()) -> Int (x, ())
  | O.Tuple (pats, ()) -> Tuple (List.map ~f:(fun pat -> desugar_pat ~pat) pats, ())
;;

let rec desugar_expr ~expr desugar =
  let module O = Ast.Renamed.Expr in
  let open Ast.Desugared.Expr in
  match expr with
  | O.Int (x, ()) -> Int (x, ())
  | O.Ident (ident, ()) -> Ident (ident, ())
  | O.Constr (ident, exprs, ()) ->
    Constr (ident, List.map ~f:(fun expr -> desugar_expr ~expr desugar) exprs, ())
  | O.Apply (expr, expr', ()) ->
    Apply (desugar_expr ~expr desugar, desugar_expr ~expr:expr' desugar, ())
  | O.Group (expr, ()) -> desugar_expr ~expr desugar
  | O.Lambda (ident, expr, ()) -> Lambda (ident, desugar_expr ~expr desugar, ())
  | O.Let (ident, expr_binding, expr_body, ()) ->
    Let
      ( ident
      , desugar_expr ~expr:expr_binding desugar
      , desugar_expr ~expr:expr_body desugar
      , () )
  | O.Match (expr_scrutinee, arms, ()) ->
    Match
      ( desugar_expr ~expr:expr_scrutinee desugar
      , List.map ~f:(fun (pat, expr) -> desugar_pat ~pat, desugar_expr ~expr desugar) arms
      , () )
  | O.Tuple (exprs, ()) ->
    Tuple (List.map ~f:(fun expr -> desugar_expr ~expr desugar) exprs, ())
  | O.BinOp (expr_left, op, expr_right, ()) ->
    (match Rename.Renamer.fetch desugar.renamer.var_renamer ~str:(bin_op_fun op) with
     | Some ident ->
       Apply
         ( Apply (Ident (ident, ()), desugar_expr ~expr:expr_left desugar, ())
         , desugar_expr ~expr:expr_right desugar
         , () )
     | None ->
       raise_s
         [%message
           "Internal compiler error: Primitive binary operator not in environment"])
;;

let rec desugar_ty ~ty =
  let module O = Ast.Renamed.Ty in
  let open Ast.Desugared.Ty in
  match ty with
  | O.Var (x, ()) -> Var (x, ())
  | O.Con (ident, tys, ()) -> Con (ident, List.map ~f:(fun ty -> desugar_ty ~ty) tys, ())
  | O.Prod (tys, ()) -> Prod (List.map ~f:(fun ty -> desugar_ty ~ty) tys, ())
  | O.Fun (ty, ty', ()) -> Fun (desugar_ty ~ty, desugar_ty ~ty:ty', ())
;;

let rec desugar_decl ~decl desugar =
  let module O = Ast.Renamed.Decl in
  let open Ast.Desugared.Decl in
  match decl with
  | O.Val (ident, expr, ()) -> Val (ident, desugar_expr ~expr desugar, ())
  | O.Type (ident, tvars, ctors, ()) ->
    Type
      ( ident
      , tvars
      , List.map
          ~f:(fun (ctor_ident, ty) ->
            ctor_ident, Option.map ~f:(fun ty -> desugar_ty ~ty) ty)
          ctors
      , () )
;;

let desugar_prog ~prog desugar = List.map ~f:(fun decl -> desugar_decl ~decl desugar) prog
