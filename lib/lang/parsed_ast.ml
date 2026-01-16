open! Base

include Ast.Make (struct
    type var_ident = string
    type ctor_ident = string
    type type_ident = string
    type tvar = string

    module Pat = struct
      type for_int = unit
      type for_ident = unit
      type for_tuple = unit
      type for_constr = unit
    end

    module Expr = struct
      type for_int = unit
      type for_ident = unit
      type for_constr = unit
      type for_apply = unit
      type for_group = unit
      type for_lambda = unit
      type for_let = unit
      type for_match = unit
      type for_tuple = unit
      type for_binop = unit
    end

    module Ty = struct
      type for_var = unit
      type for_con = unit
      type for_prod = unit
      type for_fun = unit
    end

    module Decl = struct
      type for_val = unit
      type for_type = unit
      type for_ctor = unit
    end

    module Prog = struct
      type for_decls = unit
    end
  end)

let rec equal_pat pat pat' =
  let open Pat in
  match pat, pat' with
  | Int (x, _), Int (x', _) -> equal_int x x'
  | Ident (str, _), Ident (str', _) -> equal_string str str'
  | Tuple (pats, _), Tuple (pats', _) -> Ast.equal_pairs equal_pat pats pats'
  | Constr (str, pats, _), Constr (str', pats', _) ->
    equal_string str str' && Ast.equal_pairs equal_pat pats pats'
  | _ -> false
;;

let rec equal_expr expr expr' =
  let open Expr in
  match expr, expr' with
  | Int (x, _), Int (x', _) -> equal_int x x'
  | Ident (str, _), Ident (str', _) -> equal_string str str'
  | Constr (str, exprs, _), Constr (str', exprs', _) ->
    equal_string str str' && Ast.equal_pairs equal_expr exprs exprs'
  | Apply (expr_left, expr_right, _), Apply (expr_left', expr_right', _) ->
    equal_expr expr_left expr_left' && equal_expr expr_right expr_right'
  | Group (expr, _), Group (expr', _) -> equal_expr expr expr'
  | Lambda (str, expr, _), Lambda (str', expr', _) ->
    equal_string str str' && equal_expr expr expr'
  | Let (str, expr_param, expr_body, _), Let (str', expr_param', expr_body', _) ->
    equal_string str str'
    && equal_expr expr_param expr_param'
    && equal_expr expr_body expr_body'
  | Match (expr_scrutinee, arms, _), Match (expr_scrutinee', arms', _) ->
    equal_expr expr_scrutinee expr_scrutinee'
    && Ast.equal_pairs
         (fun (pat, expr) (pat', expr') -> equal_pat pat pat' && equal_expr expr expr')
         arms
         arms'
  | Tuple (exprs, _), Tuple (exprs', _) -> Ast.equal_pairs equal_expr exprs exprs'
  | BinOp (expr_left, op, expr_right, _), BinOp (expr_left', op', expr_right', _) ->
    equal_expr expr_left expr_left'
    && (match op, op' with
        | `Plus, `Plus -> true
        | `Sub, `Sub -> true
        | `Mul, `Mul -> true
        | `Div, `Div -> true
        | _ -> false)
    && equal_expr expr_right expr_right'
  | _ -> false
;;

let rec equal_ty ty ty' =
  let open Ty in
  match ty, ty' with
  | Var (str, _), Var (str', _) -> equal_string str str'
  | Con (str, tys, _), Con (str', tys', _) ->
    equal_string str str' && Ast.equal_pairs equal_ty tys tys'
  | Prod (tys, _), Prod (tys', _) -> Ast.equal_pairs equal_ty tys tys'
  | Fun (ty_param, ty_res, _), Fun (ty_param', ty_res', _) ->
    equal_ty ty_param ty_param' && equal_ty ty_res ty_res'
  | _ -> false
;;

let equal_decl decl decl' =
  let open Decl in
  match decl, decl' with
  | Val (str, expr, _), Val (str', expr', _) ->
    equal_string str str' && equal_expr expr expr'
  | Type (str, strs, ctors, _), Type (str', strs', ctors', _) ->
    equal_string str str'
    && Ast.equal_pairs equal_string strs strs'
    && Ast.equal_pairs
         (fun (str, opt, ()) (str', opt', ()) ->
            equal_string str str' && equal_option equal_ty opt opt')
         ctors
         ctors'
  | _ -> false
;;

let equal_prog (Prog.Decls (decls, _)) (Prog.Decls (decls', _)) =
  Ast.equal_pairs equal_decl decls decls'
;;
