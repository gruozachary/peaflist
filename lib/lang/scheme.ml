open! Base

type t = Forall of Type_var.t list * Type.t [@@deriving sexp_of]

let to_string (Forall (qs, t)) =
  (List.map qs ~f:Type_var.to_string
   |> List.intersperse ~sep:" "
   |> List.fold ~init:"forall " ~f:String.append)
  ^ " . "
  ^ Type.to_string t
;;

let free_tvars = function
  | Forall (qs, ty) -> Set.diff (Set.of_list (module Type_var) qs) (Type.free_tvars ty)
;;

let of_type ty = Forall ([], ty)
