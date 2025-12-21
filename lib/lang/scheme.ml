open! Base

type t = Forall of Tvar.t list * Tau.t

let to_string (Forall (qs, t)) =
  (List.map qs ~f:Tvar.to_string
   |> List.intersperse ~sep:" "
   |> List.fold ~init:"forall " ~f:String.append)
  ^ " . "
  ^ Tau.to_string t
;;

let free_tvars = function
  | Forall (qs, ty) -> Set.diff (Set.of_list (module Tvar) qs) (Tau.free_tvars ty)
;;

let of_tau ty = Forall ([], ty)
