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

val atomic : 'a t -> 'a t
(** Returns a new parser that will not consume the input if there is an error *)

val eof : unit t
(** A parser that checks that we are at the end of the input *)

val choice : 'a t list -> 'a t
(** A parser that finds the first ok parser in a list *)

val sep_by_1 : 'a t -> sep:'b t -> 'a list t
(** Combinator that consumes a list separated by separators *)

val char : Base.char -> Base.char t
(** A parser that consumes a specific character *)

val letter : Base.char t
(** A parser that consumes a letter *)

val digit : Base.char t
(** A parser that consumes a digit *)

val space : Base.char t
(** A parser that consumes a whitespace character *)

val spaces : unit t
(** A parser that consumes whitespace characters *)

val string : Base.string -> Base.string t
(** A parser that consumes a specific string *)

val lexeme : 'a t -> 'a t
(** A parser that consumes all whitespace after the given parser *)

val fully : 'a t -> 'a t
(** A parser that consumes leading whitespace and checks for eof *)

val symbol : Base.string -> Base.string t
(** A combination of lexeme and string *)

module Let_syntax : sig
  module Let_syntax : sig
    val return : 'a -> 'a t
    val bind : 'a t -> f:('a -> 'b t) -> 'b t
    val map : 'a t -> f:('a -> 'b) -> 'b t
    val both : 'a t -> 'b t -> ('a * 'b) t

    module Open_on_rhs : sig end
  end
end
