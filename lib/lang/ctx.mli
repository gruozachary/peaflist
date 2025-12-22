type t

val empty : unit -> t

module Env : sig
  val get : t -> Term_env.t
  val map : t -> f:(Term_env.t -> Term_env.t) -> t
end

module Tenv : sig
  val get : t -> Ty_env.t
  val map : t -> f:(Ty_env.t -> Ty_env.t) -> t
end

module State : sig
  val get : t -> State.t
end
