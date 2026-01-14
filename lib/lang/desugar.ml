open! Base

type void = |

let bin_op_fun = function
  | `Plus -> "+"
  | `Mul -> "*"
  | `Sub -> "-"
  | `Div -> "/"
;;

let rec desugar_pat pat =
  let module O = Renamed_ast.Pat in
  let open Desugared_ast.Pat in
  match pat with
  | O.Constr (ident, pats, ()) -> Constr (ident, List.map ~f:desugar_pat pats, ())
  | O.Ident (ident, ()) -> Ident (ident, ())
  | O.Int (x, ()) -> Int (x, ())
  | O.Tuple (pats, ()) -> Tuple (List.map ~f:desugar_pat pats, ())
;;

let rec desugar_expr (rename_ctx : Rename.t) expr =
  let module O = Renamed_ast.Expr in
  let open Desugared_ast.Expr in
  match expr with
  | O.Int (x, ()) -> Int (x, ())
  | O.Ident (ident, ()) -> Ident (ident, ())
  | O.Constr (ident, exprs, ()) ->
    Constr (ident, List.map ~f:(desugar_expr rename_ctx) exprs, ())
  | O.Apply (expr, expr', ()) ->
    Apply (desugar_expr rename_ctx expr, desugar_expr rename_ctx expr', ())
  | O.Group (expr, ()) -> desugar_expr rename_ctx expr
  | O.Lambda (ident, expr, ()) -> Lambda (ident, desugar_expr rename_ctx expr, ())
  | O.Let (ident, expr_binding, expr_body, ()) ->
    Let
      (ident, desugar_expr rename_ctx expr_binding, desugar_expr rename_ctx expr_body, ())
  | O.Match (expr_scrutinee, arms, ()) ->
    Match
      ( desugar_expr rename_ctx expr_scrutinee
      , List.map
          ~f:(fun (pat, expr) -> desugar_pat pat, desugar_expr rename_ctx expr)
          arms
      , () )
  | O.Tuple (exprs, ()) -> Tuple (List.map ~f:(desugar_expr rename_ctx) exprs, ())
  | O.BinOp (expr_left, op, expr_right, ()) ->
    (match Rename.Renamer.fetch rename_ctx.var_renamer ~str:(bin_op_fun op) with
     | Some ident ->
       Apply
         ( Apply (Ident (ident, ()), desugar_expr rename_ctx expr_left, ())
         , desugar_expr rename_ctx expr_right
         , () )
     | None ->
       raise_s
         [%message
           "Internal compiler error: Primitive binary operator not in environment"])
;;

let rec desugar_ty ty =
  let module O = Renamed_ast.Ty in
  let open Desugared_ast.Ty in
  match ty with
  | O.Var (x, ()) -> Var (x, ())
  | O.Con (ident, tys, ()) -> Con (ident, List.map ~f:desugar_ty tys, ())
  | O.Prod (tys, ()) -> Prod (List.map ~f:desugar_ty tys, ())
  | O.Fun (ty, ty', ()) -> Fun (desugar_ty ty, desugar_ty ty', ())
;;

let desugar_decl rename_ctx decl =
  let module O = Renamed_ast.Decl in
  let open Desugared_ast.Decl in
  match decl with
  | O.Val (ident, expr, ()) -> Val (ident, desugar_expr rename_ctx expr, ())
  | O.Type (ident, tvars, ctors, ()) ->
    Type
      ( ident
      , tvars
      , List.map
          ~f:(fun (ctor_ident, ty, ()) ->
            ctor_ident, Option.map ~f:(fun ty -> desugar_ty ty) ty, ())
          ctors
      , () )
;;

let desugar_prog rename_ctx prog =
  List.map ~f:(fun decl -> desugar_decl rename_ctx decl) prog
;;
