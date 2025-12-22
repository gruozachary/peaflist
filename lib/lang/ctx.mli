type t

val empty : unit -> t

module Env : sig
  val get : t -> Term_env.t
  val map : t -> f:(Term_env.t -> Term_env.t) -> t
end

module Tenv : sig
  val get : t -> Type_env.t
  val map : t -> f:(Type_env.t -> Type_env.t) -> t
end

module State : sig
  val get : t -> State.t
end
