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
  ; mutable tvar : Type_var.t
  }

let empty () =
  { var_renamer = Renamer.empty Var_ident.create
  ; ctor_renamer = Renamer.empty Constr_ident.create
  ; type_renamer = Renamer.empty Type_ident.create
  ; tvar = Type_var.zero
  }
;;

let rec rename_pat ~pat renamer =
  let open Result in
  let open Let_syntax in
  let open Ast.Pat in
  let spawn () = Renamer.spawn renamer.var_renamer in
  let%map node, var_renamer =
    match pat.node with
    | Int x -> return (Int x, spawn ())
    | Ident str ->
      let ident, var_renamer = Renamer.fresh_add ~str (spawn ()) in
      return (Ident ident, var_renamer)
    | Tuple pats ->
      let%map pats, var_renamer =
        List.fold
          ~init:(return ([], spawn ()))
          ~f:(fun acc pat ->
            let%bind pats, var_renamer = acc in
            let%map pat, var_renamer' = rename_pat ~pat renamer in
            pat :: pats, Renamer.compose var_renamer var_renamer')
          pats
      in
      Tuple (List.rev pats), var_renamer
    | Constr (str, pats) ->
      let%bind ident =
        Renamer.fetch renamer.ctor_renamer ~str |> of_option ~error:"Unbound variable"
      in
      let%map pats, var_renamer =
        List.fold
          ~init:(return ([], spawn ()))
          ~f:(fun acc pat ->
            let%bind pats, var_renamer = acc in
            let%map pat, var_renamer' = rename_pat ~pat renamer in
            pat :: pats, Renamer.compose var_renamer var_renamer')
          pats
      in
      Constr (ident, List.rev pats), var_renamer
  in
  { node }, var_renamer
;;

let rec rename_expr ~expr renamer =
  let open Result in
  let open Let_syntax in
  let open Ast.Expr in
  let%map node =
    match expr.node with
    | Int n -> return (Int n)
    | Ident str ->
      let%map ident =
        Renamer.fetch renamer.var_renamer ~str |> of_option ~error:"Unbound variable"
      in
      Ident ident
    | Constr (str, es) ->
      let%bind ident =
        Renamer.fetch renamer.ctor_renamer ~str |> of_option ~error:"Unbound variable"
      in
      let%map es = List.map ~f:(fun expr -> rename_expr ~expr renamer) es |> all in
      Constr (ident, es)
    | Apply (expr, expr') ->
      let%bind expr = rename_expr ~expr renamer in
      let%map expr' = rename_expr ~expr:expr' renamer in
      Apply (expr, expr')
    | Group expr ->
      let%map expr = rename_expr ~expr renamer in
      Group expr
    | Lambda (str, expr) ->
      let ident, var_renamer = Renamer.fresh_add ~str renamer.var_renamer in
      let%map expr = rename_expr ~expr { renamer with var_renamer } in
      Lambda (ident, expr)
    | Let (str, expr_binding, expr_body) ->
      let ident, var_renamer = Renamer.fresh_add ~str renamer.var_renamer in
      let%bind expr_binding = rename_expr ~expr:expr_binding renamer in
      let%map expr_body = rename_expr ~expr:expr_body { renamer with var_renamer } in
      Let (ident, expr_binding, expr_body)
    | Match (expr_scrutinee, arms) ->
      let%bind expr_scrutinee = rename_expr ~expr:expr_scrutinee renamer in
      let%map arms =
        List.map
          ~f:(fun (pat, expr) ->
            let%bind pat, var_renamer = rename_pat ~pat renamer in
            let%map expr =
              rename_expr
                ~expr
                { renamer with
                  var_renamer = Renamer.compose renamer.var_renamer var_renamer
                }
            in
            pat, expr)
          arms
        |> all
      in
      Match (expr_scrutinee, arms)
    | Tuple exprs ->
      let%map exprs = List.map ~f:(fun expr -> rename_expr ~expr renamer) exprs |> all in
      Tuple exprs
    | BinOp (expr_left, op, expr_right) ->
      let%bind expr_left = rename_expr ~expr:expr_left renamer in
      let%map expr_right = rename_expr ~expr:expr_right renamer in
      BinOp (expr_left, op, expr_right)
  in
  { node }
;;
