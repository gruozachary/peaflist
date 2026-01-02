open! Base

type entry =
  { arity : int
  ; constrs : Constr_ident.t List.t
  }

type t = (Type_ident.t, entry, Type_ident.comparator_witness) Map.t

let empty : t = Map.empty (module Type_ident)
let introduce env ~id ~data = Map.set env ~key:id ~data
let lookup env ~id = Map.find env id
