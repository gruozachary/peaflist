type t

type entry =
  { arity : int
  ; constrs : Constr_ident.t List.t
  }

val empty : t
val introduce : t -> id:Type_ident.t -> data:entry -> t
val lookup : t -> id:Type_ident.t -> entry Option.t
