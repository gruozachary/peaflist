open! Base
open Lang

module Ctx = struct
  type t =
    { renamer : Var_ident.t Renamer.t
    ; renamer_heart : Var_ident.t Renamer.heart
    }
end

module Expr = struct
  type atomic
  type compound

  type _ t =
    | Int : Int.t -> atomic t
    | Var : Var_ident.t * Type.t -> atomic t
    | Lambda : Var_ident.t * Type.t * any_t -> atomic t
    | Constr : Constr_ident.t * atomic t List.t * Type.t -> compound t
    | Apply : atomic t * atomic t * Type.t -> compound t
    | Let : Var_ident.t * Scheme.t * any_t * any_t -> compound t
    | Tuple : atomic t List.t -> compound t

  (* match will be done later *)
  and any_t = Any : 'a t -> any_t

  let of_core_expr (ctx : Ctx.t) e =
    let add_binding ?ident_opt ~(e : any_t) ~t bindings =
      let ident =
        match ident_opt with
        | Option.None ->
          let ident, _ =
            Renamer.declare_and_fetch ctx.renamer ~heart:ctx.renamer_heart ~str:"v"
          in
          ident
        | Option.Some ident -> ident
      in
      Var (ident, t), (ident, e) :: bindings
    in
    let rec go ?ident_opt e =
      match e with
      | Core.Expr.Int x -> Int x, []
      | Core.Expr.Id (ident, ty) -> Var (ident, ty), []
      | Core.Expr.Constr (ctor_ident, es, t) ->
        let es_a, bindings = List.map ~f:go es |> List.unzip in
        List.concat bindings
        |> add_binding ?ident_opt ~e:(Any (Constr (ctor_ident, es_a, t))) ~t
      | Core.Expr.Apply (e, e', t) ->
        let e_a, bindings = go e
        and e_a', bindings' = go e' in
        List.append bindings bindings'
        |> add_binding ?ident_opt ~e:(Any (Apply (e_a, e_a', t))) ~t
      | Core.Expr.Lambda (ident, t, e) ->
        let e_a, bindings = go e in
        ( Lambda
            ( ident
            , t
            , List.fold
                ~init:(Any e_a)
                ~f:(fun (Any e_a_acc) (ident, e_a_bind) ->
                  Any (Let (ident, _, e_a_bind, Any e_a_acc)))
                bindings )
        , [] )
      | Core.Expr.Let (ident, scheme, e_bind, e_body) ->
        let e_bind_a, bindings = go ?ident_opt:(Option.Some ident) e_bind in
        let e_body_a, bindings' = go e_body in
        ( e_body_a
        , List.append
            (match bindings with
             | [] -> [ ident, Any e_bind_a ]
             | _ -> bindings)
            bindings' )
      | Core.Expr.Match _ -> raise_s [%message "Match not currently implemented"]
      | Core.Expr.Tuple (es, t) ->
        let es_a, bindings = List.map ~f:go es |> List.unzip in
        List.concat bindings |> add_binding ?ident_opt ~e:(Any (Tuple es_a)) ~t
    in
    let e, _ = go e in
    e
  ;;
end
