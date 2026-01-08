open! Base

type t = { renamer : Rename.t }
type void = |

let bin_op_fun = function
  | `Plus -> "+"
  | `Mul -> "*"
  | `Sub -> "-"
  | `Div -> "/"
;;

let rec desugar_pat pat =
  let module O = Ast.Renamed.Pat in
  let open Ast.Desugared.Pat in
  match pat with
  | O.Constr (ident, pats, ()) -> Constr (ident, List.map ~f:desugar_pat pats, ())
  | O.Ident (ident, ()) -> Ident (ident, ())
  | O.Int (x, ()) -> Int (x, ())
  | O.Tuple (pats, ()) -> Tuple (List.map ~f:desugar_pat pats, ())
;;

let rec desugar_expr ctx expr =
  let module O = Ast.Renamed.Expr in
  let open Ast.Desugared.Expr in
  match expr with
  | O.Int (x, ()) -> Int (x, ())
  | O.Ident (ident, ()) -> Ident (ident, ())
  | O.Constr (ident, exprs, ()) -> Constr (ident, List.map ~f:(desugar_expr ctx) exprs, ())
  | O.Apply (expr, expr', ()) -> Apply (desugar_expr ctx expr, desugar_expr ctx expr', ())
  | O.Group (expr, ()) -> desugar_expr ctx expr
  | O.Lambda (ident, expr, ()) -> Lambda (ident, desugar_expr ctx expr, ())
  | O.Let (ident, expr_binding, expr_body, ()) ->
    Let (ident, desugar_expr ctx expr_binding, desugar_expr ctx expr_body, ())
  | O.Match (expr_scrutinee, arms, ()) ->
    Match
      ( desugar_expr ctx expr_scrutinee
      , List.map ~f:(fun (pat, expr) -> desugar_pat pat, desugar_expr ctx expr) arms
      , () )
  | O.Tuple (exprs, ()) -> Tuple (List.map ~f:(desugar_expr ctx) exprs, ())
  | O.BinOp (expr_left, op, expr_right, ()) ->
    (match Rename.Renamer.fetch ctx.renamer.var_renamer ~str:(bin_op_fun op) with
     | Some ident ->
       Apply
         ( Apply (Ident (ident, ()), desugar_expr ctx expr_left, ())
         , desugar_expr ctx expr_right
         , () )
     | None ->
       raise_s
         [%message
           "Internal compiler error: Primitive binary operator not in environment"])
;;

let rec desugar_ty ty =
  let module O = Ast.Renamed.Ty in
  let open Ast.Desugared.Ty in
  match ty with
  | O.Var (x, ()) -> Var (x, ())
  | O.Con (ident, tys, ()) -> Con (ident, List.map ~f:desugar_ty tys, ())
  | O.Prod (tys, ()) -> Prod (List.map ~f:desugar_ty tys, ())
  | O.Fun (ty, ty', ()) -> Fun (desugar_ty ty, desugar_ty ty', ())
;;

let desugar_decl ctx decl =
  let module O = Ast.Renamed.Decl in
  let open Ast.Desugared.Decl in
  match decl with
  | O.Val (ident, expr, ()) -> Val (ident, desugar_expr ctx expr, ())
  | O.Type (ident, tvars, ctors, ()) ->
    Type
      ( ident
      , tvars
      , List.map
          ~f:(fun (ctor_ident, ty) ->
            ctor_ident, Option.map ~f:(fun ty -> desugar_ty ty) ty)
          ctors
      , () )
;;

let desugar_prog ctx prog = List.map ~f:(fun decl -> desugar_decl ctx decl) prog
