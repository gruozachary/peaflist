type t

val empty : unit -> t

val fetch_and_lookup : t -> ident_str:string -> (Ident.t * Scheme.t) option
val declare_and_introduce : t -> ident_str:string -> scheme:Scheme.t -> Ident.t * t

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

module Renamer : sig
  val get : t -> Renamer.t
  val map : t -> f:(Renamer.t -> Renamer.t) -> t
end
