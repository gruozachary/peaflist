open! Base
open Typing
open Match_compiled_ast

let convert_expr (rename : Rename.t) e =
  let open Expr in
  let module O = Core_ast.Unified.Expr in
  let apply_bindings e bindings =
    List.fold
      ~init:(Any e)
      ~f:(fun (Any e_a_acc) (ident, e_a_bind, scheme) ->
        Any (Let (ident, scheme, e_a_bind, Any e_a_acc)))
      bindings
  in
  let add_binding ?ident_opt ~(e : t) ~t bindings =
    let ident =
      match ident_opt with
      | None ->
        (* TODO: return updated renamer *)
        let ident, _ = Rename.Renamer.fresh_add ~str:"v" rename.var_renamer in
        ident
      | Some ident -> ident
    in
    Var (ident, t), (ident, e, Scheme.Forall ([], t)) :: bindings
  in
  let rec go ?ident_opt e =
    match e with
    | O.Int (x, ()) -> Int x, []
    | O.Ident (ident, ty) -> Var (ident, ty), []
    | O.Constr (ctor_ident, es, t) ->
      let es_a, bindings = List.map ~f:go es |> List.unzip in
      List.rev bindings
      |> List.concat
      |> add_binding ?ident_opt ~e:(Any (Constr (ctor_ident, es_a, t))) ~t
    | O.Apply (e, e', t) ->
      let e_a, bindings = go e
      and e_a', bindings' = go e' in
      List.append bindings' bindings
      |> add_binding ?ident_opt ~e:(Any (Apply (e_a, e_a', t))) ~t
    | O.Lambda (ident, e, t) ->
      let e_a, bindings = go e in
      Lambda (ident, t, apply_bindings e_a bindings), []
    | O.Let (ident, e_bind, e_body, scheme) ->
      let e_bind_a, bindings = go ?ident_opt:(Option.Some ident) e_bind in
      let e_body_a, bindings' = go e_body in
      ( e_body_a
      , List.append
          bindings'
          (match
             List.find ~f:(fun (ident', _, _) -> Var_ident.equal ident ident') bindings
           with
           | Option.None -> (ident, Any e_bind_a, scheme) :: bindings
           | Option.Some _ -> bindings) )
    | O.Match _ -> raise_s [%message "Match not currently implemented"]
    | O.Tuple (es, t) ->
      let es_a, bindings = List.map ~f:go es |> List.unzip in
      List.rev bindings
      |> List.concat
      |> add_binding ?ident_opt ~e:(Any (Tuple (es_a, t))) ~t
    | _ -> .
  in
  go e
;;
