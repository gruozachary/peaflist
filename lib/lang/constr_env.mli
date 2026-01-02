type t

type entry =
  { parent : Type_ident.t
  ; tag : int
  ; arg_scheme_opt : Scheme.t Option.t
  ; res_scheme : Scheme.t
  }

val empty : t
val introduce : t -> ident:Constr_ident.t -> data:entry -> t
val lookup : t -> ident:Constr_ident.t -> entry Option.t
