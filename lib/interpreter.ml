open! Base
open Ast

module Value = struct
  type t =
    | Int of int
    | Tuple of t list
    | Constr of string * t list
    | Closure of env * string * Expr.t

  and env = (string, t, String.comparator_witness) Map.t
end

let rec eval ~env e =
  let open Result in
  match e with
  | Expr.Apply _ -> Error "Not implemented"
  | Expr.BinOp _ -> Error "Not implemented"
  | Expr.Binding _ -> Error "Not implemented"
  | Expr.Constr _ -> Error "Not implemented"
  | Expr.Group e -> eval ~env e
  | Expr.Id x -> Ok (Map.find_exn env x)
  | Expr.Int x -> Ok (Value.Int x)
  | Expr.Lambda _ -> Error "Not implemented"
  | Expr.Match _ -> Error "Not implemented"
  | Expr.Tuple _ -> Error "Not implemented"
;;
