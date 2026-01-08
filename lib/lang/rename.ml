open! Base

module Renamer = struct
  type 'ident t =
    { create_ident : Monotonic.t -> string -> 'ident
    ; mutable monotonic : Monotonic.t
    ; map : (string, 'ident, String.comparator_witness) Map.t
    }

  let empty create_ident =
    { create_ident; monotonic = Monotonic.zero; map = Map.empty (module String) }
  ;;

  let spawn renamer = { renamer with map = Map.empty (module String) }

  let fresh ~str renamer =
    let ident = renamer.create_ident renamer.monotonic str in
    renamer.monotonic <- Monotonic.succ renamer.monotonic;
    ident
  ;;

  let fresh_add ~str renamer =
    let ident = fresh ~str renamer in
    let renamer = { renamer with map = Map.set renamer.map ~key:str ~data:ident } in
    ident, renamer
  ;;

  let fetch ~str renamer = Map.find renamer.map str

  let compose renamer renamer' =
    { renamer with
      map =
        (if phys_equal renamer.monotonic renamer'.monotonic
         then
           Map.merge renamer.map renamer'.map ~f:(fun ~key:_ -> function
             | `Left ident -> Some ident
             | `Right ident -> Some ident
             | `Both (_, ident) -> Some ident)
         else
           raise_s
             [%message "Internal compiler error: Invariant on renamer merging broken"])
    }
  ;;
end

type t =
  { var_renamer : Var_ident.t Renamer.t
  ; ctor_renamer : Constr_ident.t Renamer.t
  ; type_renamer : Type_ident.t Renamer.t
  }

let empty () =
  { var_renamer = Renamer.empty Var_ident.create
  ; ctor_renamer = Renamer.empty Constr_ident.create
  ; type_renamer = Renamer.empty Type_ident.create
  }
;;

let rec rename_pat ctx pat =
  let open Result in
  let open Let_syntax in
  let open Ast in
  let spawn () = Renamer.spawn ctx.var_renamer in
  match pat with
  | Parsed.Pat.Int (x, ()) -> return (Renamed.Pat.Int (x, ()), spawn ())
  | Parsed.Pat.Ident (str, ()) ->
    let ident, var_renamer = Renamer.fresh_add ~str (spawn ()) in
    return (Renamed.Pat.Ident (ident, ()), var_renamer)
  | Parsed.Pat.Tuple (pats, ()) ->
    let%map pats, var_renamer =
      List.fold
        ~init:(return ([], spawn ()))
        ~f:(fun acc pat ->
          let%bind pats, var_renamer = acc in
          let%map pat, var_renamer' = rename_pat ctx pat in
          pat :: pats, Renamer.compose var_renamer var_renamer')
        pats
    in
    Renamed.Pat.Tuple (List.rev pats, ()), var_renamer
  | Parsed.Pat.Constr (str, pats, ()) ->
    let%bind ident =
      Renamer.fetch ctx.ctor_renamer ~str |> of_option ~error:"Unbound variable"
    in
    let%map pats, var_renamer =
      List.fold
        ~init:(return ([], spawn ()))
        ~f:(fun acc pat ->
          let%bind pats, var_renamer = acc in
          let%map pat, var_renamer' = rename_pat ctx pat in
          pat :: pats, Renamer.compose var_renamer var_renamer')
        pats
    in
    Renamed.Pat.Constr (ident, List.rev pats, ()), var_renamer
;;

let rec rename_expr ctx expr =
  let open Result in
  let open Let_syntax in
  let open Ast in
  match expr with
  | Parsed.Expr.Int (n, ()) -> return (Renamed.Expr.Int (n, ()))
  | Parsed.Expr.Ident (str, ()) ->
    let%map ident =
      Renamer.fetch ctx.var_renamer ~str |> of_option ~error:"Unbound variable"
    in
    Renamed.Expr.Ident (ident, ())
  | Parsed.Expr.Constr (str, exprs, ()) ->
    let%bind ident =
      Renamer.fetch ctx.ctor_renamer ~str |> of_option ~error:"Unbound variable"
    in
    let%map es = List.map ~f:(rename_expr ctx) exprs |> all in
    Renamed.Expr.Constr (ident, es, ())
  | Parsed.Expr.Apply (expr, expr', ()) ->
    let%bind expr = rename_expr ctx expr in
    let%map expr' = rename_expr ctx expr' in
    Renamed.Expr.Apply (expr, expr', ())
  | Parsed.Expr.Group (expr, ()) ->
    let%map expr = rename_expr ctx expr in
    Renamed.Expr.Group (expr, ())
  | Parsed.Expr.Lambda (str, expr, ()) ->
    let ident, var_renamer = Renamer.fresh_add ~str ctx.var_renamer in
    let%map expr = rename_expr { ctx with var_renamer } expr in
    Renamed.Expr.Lambda (ident, expr, ())
  | Parsed.Expr.Let (str, expr_binding, expr_body, ()) ->
    let ident, var_renamer = Renamer.fresh_add ~str ctx.var_renamer in
    let%bind expr_binding = rename_expr ctx expr_binding in
    let%map expr_body = rename_expr { ctx with var_renamer } expr_body in
    Renamed.Expr.Let (ident, expr_binding, expr_body, ())
  | Parsed.Expr.Match (expr_scrutinee, arms, ()) ->
    let%bind expr_scrutinee = rename_expr ctx expr_scrutinee in
    let%map arms =
      List.map
        ~f:(fun (pat, expr) ->
          let%bind pat, var_renamer = rename_pat ctx pat in
          let%map expr =
            rename_expr
              { ctx with var_renamer = Renamer.compose ctx.var_renamer var_renamer }
              expr
          in
          pat, expr)
        arms
      |> all
    in
    Renamed.Expr.Match (expr_scrutinee, arms, ())
  | Parsed.Expr.Tuple (exprs, ()) ->
    let%map exprs = List.map ~f:(rename_expr ctx) exprs |> all in
    Renamed.Expr.Tuple (exprs, ())
  | Parsed.Expr.BinOp (expr_left, op, expr_right, ()) ->
    let%bind expr_left = rename_expr ctx expr_left in
    let%map expr_right = rename_expr ctx expr_right in
    Renamed.Expr.BinOp (expr_left, op, expr_right, ())
;;

let rec rename_ty ctx tvar_map ty =
  let open Result in
  let open Let_syntax in
  let open Ast in
  match ty with
  | Parsed.Ty.Var (str, ()) ->
    (match Map.find tvar_map str with
     | Some tvar -> return (Renamed.Ty.Var (tvar, ()))
     | None -> raise_s [%message "Internal compiler error: tvar not found when renaming"])
  | Parsed.Ty.Con (str, tys, ()) ->
    let%bind ident =
      Renamer.fetch ~str ctx.type_renamer |> of_option ~error:"Unbound type identifier"
    in
    let%map tys = List.map ~f:(rename_ty ctx tvar_map) tys |> all in
    Renamed.Ty.Con (ident, tys, ())
  | Parsed.Ty.Prod (tys, ()) ->
    let%map tys = List.map ~f:(rename_ty ctx tvar_map) tys |> all in
    Renamed.Ty.Prod (tys, ())
  | Parsed.Ty.Fun (ty, ty', ()) ->
    let%bind ty = rename_ty ctx tvar_map ty in
    let%map ty' = rename_ty ctx tvar_map ty' in
    Renamed.Ty.Fun (ty, ty', ())
;;

let rename_decl ctx decl =
  let open Result in
  let open Let_syntax in
  let open Ast in
  match decl with
  | Parsed.Decl.Val (str, expr, ()) ->
    let%map expr = rename_expr ctx expr in
    let ident, var_renamer = Renamer.fresh_add ~str ctx.var_renamer in
    Renamed.Decl.Val (ident, expr, ()), { ctx with var_renamer }
  | Parsed.Decl.Type (str, tvars, ctors, ()) ->
    let ident, type_renamer = Renamer.fresh_add ~str ctx.type_renamer in
    let%bind tvar_map, _ =
      List.fold
        ~init:(Some (Map.empty (module String), 0))
        ~f:(fun acc key ->
          let open Option.Let_syntax in
          let%bind map, data = acc in
          match Map.add ~key ~data map with
          | `Duplicate -> None
          | `Ok map -> Some (map, data + 1))
        tvars
      |> of_option ~error:"Duplicate type variables"
    in
    let%map ctors, ctor_renamer =
      List.fold
        ~init:(return ([], ctx.ctor_renamer))
        ~f:(fun acc (str, ty) ->
          let%bind ctors, ctor_renamer = acc in
          match ty with
          | Some ty ->
            let%map ty = rename_ty ctx tvar_map ty in
            let ident, ctor_renamer = Renamer.fresh_add ~str ctor_renamer in
            (ident, Some ty) :: ctors, ctor_renamer
          | None ->
            let ident, ctor_renamer = Renamer.fresh_add ~str ctor_renamer in
            return ((ident, None) :: ctors, ctor_renamer))
        ctors
    in
    ( Renamed.Decl.Type (ident, List.map tvars ~f:(Map.find_exn tvar_map), ctors, ())
    , { ctx with type_renamer; ctor_renamer } )
;;

let rename_prog ctx prog =
  let open Result in
  let open Let_syntax in
  List.fold
    ~init:(return ([], ctx))
    ~f:(fun acc decl ->
      let%bind decls, ctx = acc in
      let%map decl, renamer = rename_decl ctx decl in
      decl :: decls, renamer)
    prog
;;
