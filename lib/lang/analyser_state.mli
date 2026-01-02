type t

val create : unit -> t
val fresh_tv : t -> Type_var.t
val fresh : t -> Type.t
val ident_renamer_heart : t -> Ident.t Renamer.heart
val type_ident_renamer_heart : t -> Type_ident.t Renamer.heart
val constr_ident_renamer_heart : t -> Constr_ident.t Renamer.heart
