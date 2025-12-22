open! Base

type t =
  | TVar of Type_var.t
  | TFun of t * t
  | TProd of t list
  | TCon of string * t list

let prec = function
  | TVar _ -> 3
  | TFun _ -> 1
  | TProd _ -> 2
  | TCon _ -> 3
;;

let rec to_string ty =
  let go ty' = if prec ty' < prec ty then "(" ^ to_string ty' ^ ")" else to_string ty' in
  match ty with
  | TVar x -> Type_var.to_string x
  | TFun (t, t') -> go t ^ " -> " ^ go t'
  | TProd ts ->
    List.map ~f:go ts
    |> List.intersperse ~sep:" * "
    |> List.fold ~init:"" ~f:String.append
  | TCon (x, []) -> x
  | TCon (x, [ t ]) -> go t ^ " " ^ x
  | TCon (x, ts) ->
    "("
    ^ (List.map ~f:go ts
       |> List.intersperse ~sep:" "
       |> List.fold ~init:"" ~f:String.append)
    ^ ") "
    ^ x
;;

let rec free_tvars = function
  | TVar tvar -> Set.of_list (module Type_var) [ tvar ]
  | TFun (t0, t1) -> Set.union (free_tvars t0) (free_tvars t1)
  | TProd ts -> Set.union_list (module Type_var) (List.map ~f:(fun t -> free_tvars t) ts)
  | TCon (_, ts) ->
    Set.union_list (module Type_var) (List.map ~f:(fun t -> free_tvars t) ts)
;;

let rec occurs tvar t : bool =
  match t with
  | TVar x -> Type_var.equal x tvar
  | TFun (t'0, t'1) -> occurs tvar t'0 || occurs tvar t'1
  | TProd ts -> List.exists ~f:(occurs tvar) ts
  | TCon (_, ts) -> List.exists ~f:(occurs tvar) ts
;;
