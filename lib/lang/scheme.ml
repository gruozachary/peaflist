open! Base

type t = Forall of Tvar.t list * Type.t

let to_string (Forall (qs, t)) =
  (List.map qs ~f:Tvar.to_string
   |> List.intersperse ~sep:" "
   |> List.fold ~init:"forall " ~f:String.append)
  ^ " . "
  ^ Type.to_string t
;;

let free_tvars = function
  | Forall (qs, ty) -> Set.diff (Set.of_list (module Tvar) qs) (Type.free_tvars ty)
;;

let of_type ty = Forall ([], ty)
