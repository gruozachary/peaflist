open! Base

type arity = int
type t = (Type_ident.t, arity, Type_ident.comparator_witness) Map.t

let empty : t = Map.empty (module Type_ident)
let introduce env ~id ~arity = Map.set env ~key:id ~data:arity
let lookup env ~id = Map.find env id
