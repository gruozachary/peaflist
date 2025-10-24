open! Base

type alpha = string

type tau =
  | TVar of int
  | TFun of tau * tau
  | TProd of tau * tau
  | TApp of tau * tau
  | TCon of [ `Int | `Unit ]

type sigma = alpha list * tau

module Gamma : sig
  type t

  val empty : t
  val introduce : t -> alpha -> tau -> t
  val lookup : t -> alpha -> tau Option.t
end = struct
  type t = (alpha, tau, String.comparator_witness) Map.t

  let empty = Map.empty (module String)
  let introduce env tid ty = Map.add_exn env ~key:tid ~data:ty
  let lookup env tid = Map.find env tid
end

module State = struct
  type t = { mutable next : int }

  let create () = { next = 0 }

  let fresh s =
    let v = s.next in
    s.next <- v + 1;
    TVar v
  ;;
end

type error = string

type ctx =
  { env : Gamma.t
  ; state : State.t
  }

let rec typecheck_expr ~(ctx : ctx) (e : Ast.expr) : (tau, error) Result.t =
  match e with
  | Ast.Apply (_, _) -> Result.Error "Not implemented"
  | Ast.BinOp (_, _, _) -> Result.Error "Not implemented"
  | Ast.Binding (_, _, _) -> Result.Error "Not implemented"
  | Ast.Group e -> typecheck_expr ~ctx e
  | Ast.Id x -> Result.of_option (Gamma.lookup ctx.env x) ~error:"Undefined variable"
  | Ast.Int _ -> Result.Ok (TCon `Int)
  | Ast.Lambda (_, _) -> Result.Error "Not implemented"
  | Ast.List _ -> Result.Error "Not implemented"
  | Ast.Match (_, _) -> Result.Error "Not implemented"
  | Ast.Tuple es ->
    (match es with
     | [] -> Result.Ok (TCon `Unit)
     | e :: es ->
       let open Result.Let_syntax in
       let%bind t = typecheck_expr ~ctx e in
       let%map ts = Result.all (List.map ~f:(typecheck_expr ~ctx) es) in
       List.fold ~init:t ~f:(fun acc x -> TProd (acc, x)) ts)
;;
