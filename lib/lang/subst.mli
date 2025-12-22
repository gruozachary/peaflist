open! Base

type t

val empty : t
val compose : t -> t -> t
val unify : Tau.t -> Tau.t -> (t, string) Result.t
val apply_type : sub:t -> Tau.t -> Tau.t
val apply_scheme : sub:t -> Scheme.t -> Scheme.t
val apply_term_env : sub:t -> Term_env.t -> Term_env.t
