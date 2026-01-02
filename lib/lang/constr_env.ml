open! Base

type entry =
  { parent : Type_ident.t
  ; tag : int
  ; arg_scheme_opt : Scheme.t Option.t
  ; res_scheme : Scheme.t
  }

type t = (Constr_ident.t, entry, Constr_ident.comparator_witness) Map.t

let empty : t = Map.empty (module Constr_ident)
let introduce env ~ident ~data = Map.set env ~key:ident ~data
let lookup env ~ident = Map.find env ident
