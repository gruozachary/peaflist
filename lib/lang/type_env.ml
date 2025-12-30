open! Base

type arity = int
type t = (Type_ident.t, arity, Type_ident.comparator_witness) Map.t

let empty : t = Map.empty (module Type_ident)

let introduce env ~id ~arity =
  match Map.add env ~key:id ~data:arity with
  | `Duplicate -> Option.None
  | `Ok x -> Option.Some x
;;

let lookup env ~id = Map.find env id
