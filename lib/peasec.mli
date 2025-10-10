type 'a t

val exec : 'a t -> Base.string -> 'a Base.Option.t
val satisfy : (Base.char -> Base.bool) -> Base.char t
val return : 'a -> 'a t
val join : 'a t t -> 'a t
val ignore_m : 'a t -> unit t
val all : 'a t list -> 'a list t
val all_unit : unit t list -> unit t

module Let_syntax : sig
  module Let_syntax : sig
    val return : 'a -> 'a t
    val bind : 'a t -> f:('a -> 'b t) -> 'b t
    val map : 'a t -> f:('a -> 'b) -> 'b t
    val both : 'a t -> 'b t -> ('a * 'b) t

    module Open_on_rhs : sig end
  end
end
