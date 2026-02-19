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
      | GetTag of t
      | GetField of t * int * Type.unified_t
      | Switch of t * (int * t) list * t option * Type.unified_t

    let get_int : Rename.t -> Type.unified_t =
      fun rename ->
      match Rename.Renamer.fetch ~str:"int" rename.type_renamer with
      | Some ident -> Type.Con (ident, [])
      | None -> raise_s [%message "Internal compiler error: Unknown type"]
    ;;

    let rec ty_of : Rename.t -> t -> Type.phantom_unified Type.t =
      fun rename expr ->
      match expr with
      | Int _ -> get_int rename
      | Ident (_, ty) -> ty
      | Constr (_, _, ty) -> ty
      | Apply (_, _, ty) -> ty
      | Lambda (_, _, ty) -> ty
      | Let (_, _, expr_body, _) -> ty_of rename expr_body
      | Tuple (_, ty) -> ty
      | GetTag _ -> get_int rename
      | GetField (_, _, ty) -> ty
      | Switch (_, _, _, ty) -> ty
      | _ -> .
    ;;
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

let empty : unit -> ctx = fun () ->
  { tenv = Map.empty (module Type_ident); cenv = Map.empty (module Constr_ident) }
;;

module Compilation = struct
  module Ctor = struct
    module T = struct
      type t =
        { tag : int
        ; multiplicity : int
        }
      [@@deriving compare, sexp_of]
    end

    include T
    include Comparable.Make (T)
  end

  module Pattern = struct
    module T = struct
      type t =
        | Ctor of Ctor.t * t list
        | Wildcard
      [@@deriving compare, sexp_of]

      let is_wildcard : t -> bool =
        fun pat ->
        match pat with
        | Ctor _ -> false
        | Wildcard -> true
      ;;

      let of_pat : ctx -> Core_ast.Unified.Pat.t -> t =
        fun { cenv; _ } ->
        let rec go =
          fun pat ->
          match pat with
          | Core_ast.Unified.Pat.Int (x, _) -> Ctor ({ tag = x; multiplicity = 0 }, [])
          (* TODO: actually add variables *)
          | Core_ast.Unified.Pat.Ident (_, _) -> Wildcard
          | Core_ast.Unified.Pat.Tuple (pats, _) ->
            Ctor ({ tag = 0; multiplicity = List.length pats }, pats |> List.map ~f:go)
          | Core_ast.Unified.Pat.Constr (ident, pats, _) ->
            let ctor_data = Map.find_exn cenv ident in
            Ctor
              ( { tag = ctor_data.tag; multiplicity = List.length pats }
              , pats |> List.map ~f:go )
        in
        go
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
      | Leaf of Expr.t
      | Fail
      | Switch of (int * t) list * t option * Occurrence.t
      | Swap of t * int
  end

  module Matrix = struct
    type row =
      { patterns : Pattern.t list
      ; action : Expr.t
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

    let head_ctors : t -> (Ctor.t, Ctor.comparator_witness) Set.t =
      let fetch_head : row -> (Ctor.t, Ctor.comparator_witness) Set.t =
        fun { patterns; _ } ->
        match List.hd_exn patterns with
        | Pattern.Ctor (ctor, _) -> Set.singleton (module Ctor) ctor
        | _ -> Set.empty (module Ctor)
      in
      fun mat -> List.map mat ~f:fetch_head |> Set.union_list (module Ctor)
    ;;

    let specialise : t -> Ctor.t -> t =
      fun mat ctor ->
      mat
      |> List.filter_map ~f:(fun row ->
        let pat_hd, pats_tl = List.hd_exn row.patterns, List.tl_exn row.patterns in
        match pat_hd with
        | Pattern.Ctor (ctor', ctor_pats) when Ctor.equal ctor ctor' ->
          Some { row with patterns = List.append ctor_pats pats_tl }
        | Pattern.Wildcard ->
          let pats_hd =
            Sequence.unfold ~init:ctor.multiplicity ~f:(fun i ->
              if i > 0 then Some (Pattern.Wildcard, i - 1) else None)
            |> Sequence.to_list
          in
          Some { row with patterns = List.append pats_hd pats_tl }
        | _ -> None)
    ;;
  end

  let rec compile : Occurrence.t list -> Matrix.t -> Tree.t =
    let swap_oc_to_front : int -> Occurrence.t list -> Occurrence.t list =
      fun i ocs ->
      let xs, ys = List.split_n ocs i in
      List.hd_exn ys :: xs |> List.append (List.tl_exn ys)
    in
    fun ocs mat ->
      match mat with
      | [] -> Tree.Fail
      | row :: _ when Matrix.row_all_wildcards row -> Tree.Leaf row.action
      | _ ->
        let selected_col = Matrix.find_good_column mat in
        let mat = Matrix.swap_col_to_front selected_col mat in
        let ocs = swap_oc_to_front selected_col ocs in
        let oc_hd, ocs_tl = List.hd_exn ocs, List.tl_exn ocs in
        let ctors = Matrix.head_ctors mat in
        let subtrees =
          ctors
          |> Set.to_list
          |> List.map ~f:(fun ctor ->
            let mat = Matrix.specialise mat ctor in
            let ocs_hd =
              Sequence.unfold ~init:1 ~f:(fun i ->
                if i <= ctor.multiplicity
                then Some (Occurrence.Path (i, oc_hd), i + 1)
                else None)
              |> Sequence.to_list
            in
            ctor.tag, compile (List.append ocs_hd ocs_tl) mat)
        in
        (* TODO: Default matrix *)
        Tree.Switch (subtrees, None, oc_hd)
  ;;
end

let rec convert_expr
  : Rename.t -> ctx -> Core_ast.Unified.Expr.t -> (Expr.t, string) Result.t
  =
  fun rename ->
  let wrap_in_let
    : Expr.t -> (Expr.t -> (Expr.t, string) Result.t) -> (Expr.t, string) Result.t
    =
    fun expr_assignee f ->
    let ident = Rename.Renamer.fresh ~str:"v" rename.var_renamer in
    let%map expr_body = f (Expr.Ident (ident, Expr.ty_of rename expr_assignee)) in
    let scheme = Scheme.Forall ([], Expr.ty_of rename expr_body) in
    Expr.Let (ident, expr_assignee, expr_body, scheme)
  in
  let rec oc_to_expr : Compilation.Occurrence.t -> Expr.t -> Expr.t =
    fun oc expr ->
    match oc with
    | Compilation.Occurrence.Root -> expr
    | Compilation.Occurrence.Path (field_idx, oc') ->
      let ty = Expr.ty_of rename expr in
      let ty_next =
        match ty with
        | Type.Gen _ -> assert false
        | Type.Fun (_, _) -> assert false
        | Type.Prod tys -> List.nth_exn tys field_idx
        | Type.Con (_, tys) -> List.nth_exn tys field_idx
        | Type.Uni _ -> assert false
      in
      Expr.GetField (expr, field_idx, ty_next) |> oc_to_expr oc'
  in
  let rec tree_to_expr
    : Expr.t -> Type.unified_t -> Compilation.Tree.t -> (Expr.t, string) Result.t
    =
    fun root result_ty tree ->
    match tree with
    | Compilation.Tree.Leaf expr -> return expr
    | Compilation.Tree.Fail ->
      raise_s [%message "Internal compiler error: Fail node in match tree"]
    | Compilation.Tree.Switch (branches, default, oc) ->
      wrap_in_let (oc_to_expr oc root) (fun e ->
        let%bind branches =
          branches
          |> List.fold_result ~init:[] ~f:(fun acc (i, subtree) ->
            let%map expr_subtree = tree_to_expr root result_ty subtree in
            (i, expr_subtree) :: acc)
          >>| List.rev
        in
        let%map default =
          match default with
          | None -> return None
          | Some subtree ->
            let%map expr_subtree = tree_to_expr root result_ty subtree in
            Some expr_subtree
        in
        Expr.Switch (Expr.GetTag e, branches, default, result_ty))
    | Compilation.Tree.Swap (tree, _) -> tree_to_expr root result_ty tree
  in
  fun ctx ->
    let module O = Core_ast.Unified.Expr in
    let open Expr in
    let convert_exprs exprs =
      let%map exprs =
        List.fold_result exprs ~init:[] ~f:(fun acc expr ->
          let%map expr = convert_expr rename ctx expr in
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
      let%bind expr_fun = convert_expr rename ctx expr_fun in
      let%map expr_arg = convert_expr rename ctx expr_arg in
      Apply (expr_fun, expr_arg, ty)
    | O.Lambda (ident, expr_body, ty) ->
      let%map expr_body = convert_expr rename ctx expr_body in
      Lambda (ident, expr_body, ty)
    | O.Let (ident, expr_binding, expr_body, scheme) ->
      let%bind expr_binding = convert_expr rename ctx expr_binding in
      let%map expr_body = convert_expr rename ctx expr_body in
      Let (ident, expr_binding, expr_body, scheme)
    | O.Match (expr_scrutinee, arms, ty) ->
      let%bind expr_scrutinee = convert_expr rename ctx expr_scrutinee in
      let%bind mat =
        arms
        |> List.fold_result ~init:[] ~f:(fun acc (pat, expr) ->
          let%map action = convert_expr rename ctx expr in
          let patterns = [ Compilation.Pattern.of_pat ctx pat ] in
          { Compilation.Matrix.patterns; action } :: acc)
        >>| List.rev
      in
      let ocs = [ Compilation.Occurrence.Root ] in
      let tree = Compilation.compile ocs mat in
      tree_to_expr expr_scrutinee ty tree
    | O.Tuple (exprs, ty) ->
      let%map exprs = convert_exprs exprs in
      Tuple (exprs, ty)
    | _ -> .
;;

let convert_decl
  : Rename.t -> ctx -> Core_ast.Unified.Decl.t -> (Decl.t * ctx, string) Result.t
  =
  fun rename ctx -> function
  | Core_ast.Unified.Decl.Val (ident, expr, scheme) ->
    let%map expr = convert_expr rename ctx expr in
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

let convert_prog
  : Rename.t -> ctx -> Core_ast.Unified.Prog.t -> (Prog.t * ctx, string) Result.t
  =
  fun rename ctx (Core_ast.Unified.Prog.Decls (decls, ())) ->
  let%map decls, ctx =
    List.fold_result
      ~init:([], ctx)
      ~f:(fun (decls_acc, ctx) decl ->
        let%map decl, ctx = convert_decl rename ctx decl in
        decl :: decls_acc, ctx)
      decls
  in
  Prog.Decls (List.rev decls, ()), ctx
;;
