type 'a t
(** The parser type.*)

val exec : 'a t -> Base.string -> 'a Base.Option.t
(** A function that executes a parser with a given string*)

val satisfy : (Base.char -> Base.bool) -> Base.char t
(** A parser that checks if the next character against a predicate *)

val return : 'a -> 'a t
(** Create a parser with any value *)

val join : 'a t t -> 'a t
(** Flatten a parser of a parser into one parser *)

val ignore_m : 'a t -> unit t
(** Ignores the value of a parser *)

val all : 'a t list -> 'a list t
(** Converts a list of parsers into a parser with a list *)

val all_unit : unit t list -> unit t
(** Discards the result of a list of parsers *)

val first_ok : 'a t -> 'a t -> 'a t
(** Gets the first non-failing parser *)

val many : 'a t -> 'a list t
(** Creates a parser that repeatedly applies a parser until failure *)

module Let_syntax : sig
  module Let_syntax : sig
    val return : 'a -> 'a t
    val bind : 'a t -> f:('a -> 'b t) -> 'b t
    val map : 'a t -> f:('a -> 'b) -> 'b t
    val both : 'a t -> 'b t -> ('a * 'b) t

    module Open_on_rhs : sig end
  end
end
