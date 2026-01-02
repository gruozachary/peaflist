open! Base

type t = (Var_ident.t, Scheme.t, Var_ident.comparator_witness) Map.t

module Merge_element = struct
  include Map.Merge_element

  type nonrec t = (Scheme.t, Scheme.t) t
end

let empty () = Map.empty (module Var_ident)
let introduce env ~id ~sc = Map.set env ~key:id ~data:sc
let lookup env ~id = Map.find env id
let map env ~f = Map.map ~f env

let free_tvars env =
  Map.fold
    env
    ~init:(Set.empty (module Type_var))
    ~f:(fun ~key:_ ~data acc -> Set.union acc (Scheme.free_tvars data))
;;

let merge = Map.merge
let size = Map.length
