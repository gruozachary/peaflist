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
    | Lambda : Var_ident.t * Type.t * 'any t -> atomic t
    | Constr : Constr_ident.t * atomic t List.t * Type.t -> compound t
    | Apply : atomic t * atomic t * Type.t -> compound t
    | Let : Var_ident.t * Scheme.t * 'any t * 'any t -> compound t
    | Tuple : atomic t List.t -> compound t
  (* match will be done later *)

  let of_core_expr (ctx : Ctx.t) e =
    let add_binding ~(e : compound t) ~t bindings =
      let ident, _ =
        Renamer.declare_and_fetch ctx.renamer ~heart:ctx.renamer_heart ~str:"v"
      in
      Var (ident, t), (ident, e) :: bindings
    in
    let rec go e =
      match e with
      | Core.Expr.Int x -> Int x, []
      | Core.Expr.Id (ident, ty) -> Var (ident, ty), []
      | Core.Expr.Constr (ctor_ident, es, t) ->
        let es_a, bindings = List.map ~f:go es |> List.unzip in
        List.concat bindings |> add_binding ~e:(Constr (ctor_ident, es_a, t)) ~t
      | Core.Expr.Apply (e, e', t) ->
        let e_a, bindings = go e
        and e_a', bindings' = go e' in
        List.append bindings bindings' |> add_binding ~e:(Apply (e_a, e_a', t)) ~t
      | Core.Expr.Lambda _ -> _
      | Core.Expr.Let _ -> _
      | Core.Expr.Match _ -> raise_s [%message "Match not currently implemented"]
      | Core.Expr.Tuple (es, t) ->
        let es_a, bindings = List.map ~f:go es |> List.unzip in
        List.concat bindings |> add_binding ~e:(Tuple es_a) ~t
    in
    let e, _ = go e in
    e
  ;;
end
