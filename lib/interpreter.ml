open! Base
open Ast

module rec Env : sig
  type t

  val lookup : t -> id:string -> Value.t
  val empty : t
end = struct
  type t = (string, Value.t, String.comparator_witness) Map.t

  let lookup env ~id = Map.find_exn env id
  let empty = Map.empty (module String)
end

and Value : sig
  type t =
    | Int of int
    | Tuple of t list
    | Constr of string * t list
    | Closure of Env.t * string * Expr.t

  val to_string : t -> string
end = struct
  type t =
    | Int of int
    | Tuple of t list
    | Constr of string * t list
    | Closure of Env.t * string * Expr.t

  let rec to_string = function
    | Int x -> Int.to_string x
    | Tuple vs ->
      (List.map ~f:to_string vs
       |> List.intersperse ~sep:", "
       |> List.fold ~init:"(" ~f:String.append)
      ^ ")"
    | Constr _ -> "CONSTRUCTOR"
    | Closure _ -> "CLOSURE"
  ;;
end

let rec eval ~env e =
  let open Result in
  match e with
  | Expr.Apply _ -> Error "Not implemented"
  | Expr.BinOp _ -> Error "Not implemented"
  | Expr.Binding _ -> Error "Not implemented"
  | Expr.Constr _ -> Error "Not implemented"
  | Expr.Group e -> eval ~env e
  | Expr.Id x -> Ok (Env.lookup env ~id:x)
  | Expr.Int x -> Ok (Value.Int x)
  | Expr.Lambda _ -> Error "Not implemented"
  | Expr.Match _ -> Error "Not implemented"
  | Expr.Tuple _ -> Error "Not implemented"
;;
