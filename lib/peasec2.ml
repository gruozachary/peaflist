open! Base

type 'a t = {
  unparser :
    'r.
    string ->
    ('a -> string -> unit -> 'r) ->
    ('a -> unit -> 'r) ->
    (string -> unit -> 'r) ->
    (unit -> 'r) ->
    'r;
}

(* 'a - What this parser gives to it's next continuations *)
(* 'r - What the next continuation returns *)

let satisfy (f : char -> bool) : char t =
  {
    unparser =
      (fun inp cok _ _ eerr ->
        if String.length inp > 0 && f inp.[0] then
          cok inp.[0] (String.drop_prefix inp 1) ()
        else eerr ());
  }

let exec p inp =
  p.unparser inp
    (fun x _ () -> Option.Some x)
    (fun x () -> Option.Some x)
    (fun _ () -> Option.None)
    (fun () -> Option.None)

module Peasec_monad_basic = struct
  type nonrec 'a t = 'a t

  let return x = { unparser = (fun _ _ eok _ _ -> eok x ()) }

  let bind p ~f =
    {
      unparser =
        (fun inp cok eok cerr eerr ->
          p.unparser inp
            (fun x inp' () ->
              (f x).unparser inp' cok (fun x -> cok x inp') cerr (cerr inp'))
            (fun x () -> (f x).unparser inp cok eok cerr eerr)
            cerr eerr);
    }

  let map =
    `Custom
      (fun p ~f ->
        {
          unparser =
            (fun inp cok eok cerr eerr ->
              p.unparser inp
                (fun x inp' -> cok (f x) inp')
                (fun x -> eok (f x))
                cerr eerr);
        })
end

module M = Monad.Make (Peasec_monad_basic)

module A = Applicative.Of_monad (struct
  type nonrec 'a t = 'a t

  include M
end)

include M
include A

let empty = { unparser = (fun _ _ _ _ eerr -> eerr ()) }

let ( <|> ) lp rp =
  {
    unparser =
      (fun inp cok eok cerr eerr ->
        lp.unparser inp cok eok cerr (fun () ->
            rp.unparser inp cok eok cerr eerr));
  }

let choice = List.fold ~init:empty ~f:( <|> )

(* Combinators which are more string-related *)

let char c = satisfy (equal_char c)
let letter = satisfy Char.is_alpha
let digit = satisfy Char.is_digit
let ws = satisfy Char.is_whitespace
