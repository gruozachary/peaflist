open! Base

type alpha = string

type concrete =
  | Int
  | Unit
[@@deriving eq]

type tau =
  | TVar of alpha
  | TFun of tau * tau
  | TProd of tau list
  | TApp of alpha * tau list
  | TCon of concrete

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

let rec occurs (tvar : alpha) (t : tau) : bool =
  match t with
  | TVar x -> equal_string x tvar
  | TFun (t'0, t'1) -> occurs tvar t'0 || occurs tvar t'1
  | TProd ts -> List.exists ~f:(occurs tvar) ts
  | TApp (tvar', ts) -> equal_string tvar tvar' || List.exists ~f:(occurs tvar) ts
  | TCon _ -> false
;;

module State = struct
  type t = { mutable next : int }

  let create () = { next = 0 }

  let fresh s =
    let v = s.next in
    s.next <- v + 1;
    TVar ("_'_tvar" ^ Int.to_string v)
  ;;
end

module Subst = struct
  type t = (alpha, tau, String.comparator_witness) Map.t

  let rec apply ~sub t =
    match t with
    | TVar x ->
      (match Map.find sub x with
       | Some t' -> t'
       | None -> t)
    | TFun (t'0, t'1) -> TFun (apply ~sub t'0, apply ~sub t'1)
    | TProd t's -> TProd (List.map t's ~f:(apply ~sub))
    | TApp (tvar, t's) -> TApp (tvar, List.map t's ~f:(apply ~sub))
    | TCon _ -> t
  ;;

  let add ~sub tvar t =
    match t with
    | TVar x ->
      if equal_string x tvar
      then Result.Ok sub
      else Result.Ok (Map.add_exn sub ~key:tvar ~data:t)
    | _ ->
      if occurs tvar t
      then Result.Error "Recursive type variable definiton"
      else Result.Ok (Map.add_exn sub ~key:tvar ~data:t)
  ;;
end

type error = string

type ctx =
  { env : Gamma.t
  ; state : State.t
  ; subst : Subst.t
  }

let rec typecheck_expr ~(ctx : ctx) (e : Ast.expr) : (tau, error) Result.t =
  match e with
  | Ast.Apply (_, _) -> Result.Error "Not implemented"
  | Ast.BinOp (_, _, _) -> Result.Error "Not implemented"
  | Ast.Binding (_, _, _) -> Result.Error "Not implemented"
  | Ast.Group e -> typecheck_expr ~ctx e
  | Ast.Id x -> Result.of_option (Gamma.lookup ctx.env x) ~error:"Undefined variable"
  | Ast.Int _ -> Result.Ok (TCon Int)
  | Ast.Lambda (_, _) -> Result.Error "Not implemented"
  | Ast.List _ -> Result.Error "Not implemented"
  | Ast.Match (_, _) -> Result.Error "Not implemented"
  | Ast.Tuple es ->
    (match es with
     | [] -> Result.Ok (TCon Unit)
     | es ->
       let open Result.Let_syntax in
       let%map ts = Result.all (List.map ~f:(typecheck_expr ~ctx) es) in
       TProd ts)
;;
