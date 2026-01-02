type t

val empty : unit -> t
val fetch_and_lookup : t -> ident_str:string -> (Var_ident.t * Scheme.t) option
val declare_and_introduce : t -> ident_str:string -> scheme:Scheme.t -> Var_ident.t * t

val type_fetch_and_lookup
  :  t
  -> ident_str:string
  -> (Type_ident.t * Type_env.entry) option

val type_declare_and_introduce
  :  t
  -> ident_str:string
  -> entry:Type_env.entry
  -> (Type_ident.t * t) Option.t

val constr_fetch_and_lookup
  :  t
  -> ident_str:string
  -> (Constr_ident.t * Constr_env.entry) option

val constr_declare_and_introduce
  :  t
  -> ident_str:string
  -> entry:Constr_env.entry
  -> Constr_ident.t * t

module Env : sig
  val get : t -> Term_env.t
  val map : t -> f:(Term_env.t -> Term_env.t) -> t
end

module Tenv : sig
  val get : t -> Type_env.t
  val map : t -> f:(Type_env.t -> Type_env.t) -> t
end

module C_env : sig
  val get : t -> Constr_env.t
  val map : t -> f:(Constr_env.t -> Constr_env.t) -> t
end

module State : sig
  val get : t -> Analyser_state.t
end

module Var_ident_renamer : sig
  val get : t -> Var_ident.t Renamer.t
  val map : t -> f:(Var_ident.t Renamer.t -> Var_ident.t Renamer.t) -> t
end

module Type_ident_renamer : sig
  val get : t -> Type_ident.t Renamer.t
  val map : t -> f:(Type_ident.t Renamer.t -> Type_ident.t Renamer.t) -> t
end

module Constr_ident_renamer : sig
  val get : t -> Constr_ident.t Renamer.t
  val map : t -> f:(Constr_ident.t Renamer.t -> Constr_ident.t Renamer.t) -> t
end
