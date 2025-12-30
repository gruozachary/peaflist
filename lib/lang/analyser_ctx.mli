type t

val empty : unit -> t
val fetch_and_lookup : t -> ident_str:string -> (Ident.t * Scheme.t) option
val declare_and_introduce : t -> ident_str:string -> scheme:Scheme.t -> Ident.t * t

val type_fetch_and_lookup
  :  t
  -> ident_str:string
  -> (Type_ident.t * Type_env.arity) option

val type_declare_and_introduce
  :  t
  -> ident_str:string
  -> arity:Type_env.arity
  -> (Type_ident.t * t) Option.t

module Env : sig
  val get : t -> Term_env.t
  val map : t -> f:(Term_env.t -> Term_env.t) -> t
end

module Tenv : sig
  val get : t -> Type_env.t
  val map : t -> f:(Type_env.t -> Type_env.t) -> t
end

module State : sig
  val get : t -> Analyser_state.t
end

module Ident_renamer : sig
  val get : t -> Ident.t Renamer.t
  val map : t -> f:(Ident.t Renamer.t -> Ident.t Renamer.t) -> t
end

module Type_ident_renamer : sig
  val get : t -> Type_ident.t Renamer.t
  val map : t -> f:(Type_ident.t Renamer.t -> Type_ident.t Renamer.t) -> t
end
