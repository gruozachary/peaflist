(** The parser type.*)
type 'a t

(** A function that executes a parser with a given string*)
val exec : 'a t -> Base.string -> 'a Base.Option.t

val fail : 'a t
(* Unconditionally fails *)

(** A parser that checks if the next character against a predicate *)
val satisfy : (Base.char -> Base.bool) -> Base.char t

(** Create a parser with any value *)
val return : 'a -> 'a t

(** Flatten a parser of a parser into one parser *)
val join : 'a t t -> 'a t

(** Ignores the value of a parser *)
val ignore_m : 'a t -> unit t

(** Converts a list of parsers into a parser with a list *)
val all : 'a t list -> 'a list t

(** Discards the result of a list of parsers *)
val all_unit : unit t list -> unit t

(** Gets the first non-failing parser *)
val ( <|> ) : 'a t -> 'a t -> 'a t

(** Infix map *)
val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t

(** Infix bind*)
val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

(** Infix ap *)
val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t

(** Explicit ignore left *)
val ( *> ) : unit t -> 'a t -> 'a t

(** Explicit ignore right *)
val ( <* ) : 'a t -> unit t -> 'a t

(** Implicit ignore left *)
val ( >*> ) : 'a t -> 'b t -> 'b t

(** Implicit ignore right *)
val ( <*< ) : 'a t -> 'b t -> 'a t

(** Creates a parser that repeatedly applies a parser until failure *)
val many : 'a t -> 'a list t

(** Creates a parser that repeatedly applies a parser until failure, at least
    one result *)
val some : 'a t -> 'a list t

(** Returns a new parser that will not consume the input if there is an error *)
val attempt : 'a t -> 'a t

(** A parser that checks that we are at the end of the input *)
val eof : unit t

(** A parser that attempts to run or returns a default value *)
val option : 'a t -> def:'a -> 'a t

(** Stops eager evaluation when building the parser *)
val defer : (unit -> 'a t) -> 'a t

(** A parser that finds the first ok parser in a list *)
val choice : 'a t list -> 'a t

(** Negative lookahead *)
val not_followed_by : 'a t -> unit t

(** Positive lookahead *)
val followed_by : 'a t -> unit t

(** Combinator that consumes a list separated by separators (at least one)*)
val sep_by_1 : 'a t -> sep:'b t -> 'a list t

(** Combinator that consumes a list separated by separators *)
val sep_by : 'a t -> sep:'b t -> 'a list t

(** Parser that gets the result of a parser between two others *)
val between : l:'b t -> 'a t -> r:'c t -> 'a t

(** Consumes values in a left-associative way *)
val chain_left_1 : 'a t -> ('a -> 'a -> 'a) t -> 'a t

(** Consumes values in a right-associative way *)
val chain_right_1 : 'a t -> ('a -> 'a -> 'a) t -> 'a t

(** A parser that consumes a specific character *)
val char : Base.char -> Base.char t

(** A parser that consumes a letter *)
val letter : Base.char t

(** A parser that consumes a digit *)
val digit : Base.char t

(** A parser that consumes a whitespace character *)
val space : Base.char t

(** A parser that consumes whitespace characters *)
val spaces : unit t

(** A parser that consumes at least 1 whitespace character *)
val spaces_1 : unit t

(** A parser that consumes a specific string *)
val string : Base.string -> Base.string t

(** A parser that consumes all whitespace after the given parser *)
val lexeme : 'a t -> 'a t

(** A parser that consumes leading whitespace and checks for eof *)
val fully : 'a t -> 'a t

module Let_syntax : sig
  module Let_syntax : sig
    val return : 'a -> 'a t
    val bind : 'a t -> f:('a -> 'b t) -> 'b t
    val map : 'a t -> f:('a -> 'b) -> 'b t
    val both : 'a t -> 'b t -> ('a * 'b) t

    module Open_on_rhs : sig end
  end
end
