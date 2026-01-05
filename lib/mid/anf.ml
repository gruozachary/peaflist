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
    let rec go e =
      match e with
      | Core.Expr.Int x -> Int x, []
      | Core.Expr.Id (ident, ty) -> Var (ident, ty), []
      | Core.Expr.Constr _ -> _
      | Core.Expr.Apply _ -> _
      | Core.Expr.Lambda _ -> _
      | Core.Expr.Let _ -> _
      | Core.Expr.Match _ -> raise_s [%message "Match not currently implemented"]
      | Core.Expr.Tuple (es, t) ->
        let es, bindings = List.map ~f:go es |> List.unzip in
        let ident, _ =
          Renamer.declare_and_fetch ctx.renamer ~heart:ctx.renamer_heart ~str:"v"
        in
        Var (ident, t), (ident, Tuple es) :: List.concat bindings
    in
    let e, _ = go e in
    e
  ;;
end
