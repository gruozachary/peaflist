open! Base
open Typing

module Ast = struct
  module Expr = struct
    type t =
      | Int of int
      | Ident of Var_ident.t * Type.unified_t
      | Constr of Constr_ident.t * t List.t * Type.unified_t
      | Apply of t * t * Type.unified_t
      | Lambda of Var_ident.t * t * Type.unified_t
      | Let of Var_ident.t * t * t * Scheme.unified_t
      | Tuple of t List.t * Type.unified_t
      | GetTag of Var_ident.t
      | GetField of Var_ident.t * int
      | Switch of t * (int * t) list * t option * Type.unified_t
  end

  module Ty = Core_ast.Unified.Ty

  module Decl = Ast.Make_decl (struct
      module Expr = Expr
      module Ty = Ty

      type var_ident = Var_ident.t
      type type_ident = Type_ident.t
      type ctor_ident = Constr_ident.t
      type tvar = Gen_var.t
      type for_val = Scheme.unified_t
      type for_type = Core_ast.Unified.type_data
      type for_ctor = Core_ast.Unified.ctor_data
    end)

  module Prog = Ast.Make_prog (struct
      module Decl = Decl

      type for_decls = unit
    end)
end

open Ast
open Result.Let_syntax

type ctx =
  { tenv : (Type_ident.t, Core_ast.Unified.type_data, Type_ident.comparator_witness) Map.t
  ; cenv :
      (Constr_ident.t, Core_ast.Unified.ctor_data, Constr_ident.comparator_witness) Map.t
  }

module Compilation = struct
  module Pattern = struct
    module T = struct
      type ctor_data =
        { tag : int
        ; multiplicity : int
        }
      [@@deriving compare, sexp_of]

      type t =
        | Ctor of ctor_data
        | Wildcard
      [@@deriving compare, sexp_of]

      let is_wildcard : t -> bool =
        fun pat ->
        match pat with
        | Ctor _ -> false
        | Wildcard -> true
      ;;
    end

    include T
    include Comparable.Make (T)
  end

  module Occurrence = struct
    type t =
      | Root
      | Path of int * t
  end

  module Tree = struct
    type t =
      | Leaf of Core_ast.Unified.Expr.t
      | Fail
      | Switch of (int * t) list * t option * Occurrence.t
      | Swap of t * int
  end

  module Matrix = struct
    type row =
      { patterns : Pattern.t list
      ; action : Core_ast.Unified.Expr.t
      }

    type t = row list

    let row_all_wildcards : row -> bool =
      fun { patterns; _ } -> List.for_all patterns ~f:Pattern.is_wildcard
    ;;

    let find_good_column : t -> int =
      fun mat ->
      match mat with
      | [] -> assert false
      | row :: _ ->
        List.find_mapi row.patterns ~f:(fun ix pat ->
          if Pattern.is_wildcard pat then None else Some ix)
        |> Option.value ~default:0
    ;;

    let swap_col_to_front : int -> t -> t =
      let swap_row : int -> row -> row =
        fun i row ->
        let xs, ys = List.split_n row.patterns i in
        { row with patterns = List.hd_exn ys :: xs |> List.append (List.tl_exn ys) }
      in
      fun i mat -> List.map ~f:(swap_row i) mat
    ;;
  end

  let compile : Matrix.t -> Occurrence.t list -> Tree.t =
    let swap_oc_to_front : int -> Occurrence.t list -> Occurrence.t list =
      fun i ocs ->
      let xs, ys = List.split_n ocs i in
      List.hd_exn ys :: xs |> List.append (List.tl_exn ys)
    in
    fun mat ocs ->
      match mat with
      | [] -> Tree.Fail
      | row :: _ when Matrix.row_all_wildcards row -> _
      | _ ->
        let selected_col = Matrix.find_good_column mat in
        let mat = Matrix.swap_col_to_front selected_col mat in
        let ocs=  swap_oc_to_front selected_col ocs in _
  ;;
end

let rec convert_expr : ctx -> Core_ast.Unified.Expr.t -> (Expr.t, string) Result.t =
  fun ctx ->
  let module O = Core_ast.Unified.Expr in
  let open Expr in
  let convert_exprs exprs =
    let%map exprs =
      List.fold_result exprs ~init:[] ~f:(fun acc expr ->
        let%map expr = convert_expr ctx expr in
        expr :: acc)
    in
    List.rev exprs
  in
  function
  | O.Int (x, _) -> return (Int x)
  | O.Ident (ident, ty) -> return (Ident (ident, ty))
  | O.Constr (ident, exprs, ty) ->
    let%map exprs = convert_exprs exprs in
    Constr (ident, exprs, ty)
  | O.Apply (expr_fun, expr_arg, ty) ->
    let%bind expr_fun = convert_expr ctx expr_fun in
    let%map expr_arg = convert_expr ctx expr_arg in
    Apply (expr_fun, expr_arg, ty)
  | O.Lambda (ident, expr_body, ty) ->
    let%map expr_body = convert_expr ctx expr_body in
    Lambda (ident, expr_body, ty)
  | O.Let (ident, expr_binding, expr_body, scheme) ->
    let%bind expr_binding = convert_expr ctx expr_binding in
    let%map expr_body = convert_expr ctx expr_body in
    Let (ident, expr_binding, expr_body, scheme)
  | O.Match (expr_scrutinee, arms, ty) -> _
  | O.Tuple (exprs, ty) ->
    let%map exprs = convert_exprs exprs in
    Tuple (exprs, ty)
  | _ -> .
;;

let convert_decl : ctx -> Core_ast.Unified.Decl.t -> (Decl.t * ctx, string) Result.t =
  fun ctx -> function
  | Core_ast.Unified.Decl.Val (ident, expr, scheme) ->
    let%map expr = convert_expr ctx expr in
    Decl.Val (ident, expr, scheme), ctx
  | Core_ast.Unified.Decl.Type (ident, gen_vars, ctors, type_data) ->
    let tenv = Map.set ctx.tenv ~key:ident ~data:type_data in
    let cenv =
      List.fold
        ~init:ctx.cenv
        ~f:(fun cenv (ident_c, _, ctor_data) -> Map.set cenv ~key:ident_c ~data:ctor_data)
        ctors
    in
    return (Decl.Type (ident, gen_vars, ctors, type_data), { tenv; cenv })
;;

let convert_prog : ctx -> Core_ast.Unified.Prog.t -> (Prog.t * ctx, string) Result.t =
  fun ctx (Core_ast.Unified.Prog.Decls (decls, ())) ->
  let%map decls, ctx =
    List.fold_result
      ~init:([], ctx)
      ~f:(fun (decls_acc, ctx) decl ->
        let%map decl, ctx = convert_decl ctx decl in
        decl :: decls_acc, ctx)
      decls
  in
  Prog.Decls (List.rev decls, ()), ctx
;;
