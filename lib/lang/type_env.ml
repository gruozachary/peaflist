open! Base

type arity = int
type t = (string, arity, String.comparator_witness) Map.t

let empty : t = Map.empty (module String)
let introduce env ~id ~arity = Map.set env ~key:id ~data:arity
let lookup env ~id = Map.find env id
