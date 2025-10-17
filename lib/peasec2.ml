open! Base

type ('a, 'r) t =
  | P of
      (string ->
      ('a -> string -> unit -> 'r) ->
      ('a -> unit -> 'r) ->
      (string -> unit -> 'r) ->
      (unit -> 'r) ->
      'r)

let unparser p inp cok eok cerr eerr =
  match p with P f -> f inp cok eok cerr eerr

(* 'a - What this parser gives to it's next continuations *)
(* 'r - What the next continuation returns *)

let satisfy (f : char -> bool) : (char, 'r) t =
  P
    (fun inp cok _ _ eerr ->
      if String.length inp > 0 && f inp.[0] then
        cok inp.[0] (String.drop_prefix inp 1) ()
      else eerr ())

let exec p inp =
  unparser p inp
    (fun x _ () -> Option.Some x)
    (fun x () -> Option.Some x)
    (fun _ () -> Option.None)
    (fun () -> Option.None)

module Peasec_monad_basic = struct
  type nonrec ('a, 'r) t = ('a, 'r) t

  let return x = P (fun _ _ eok _ _ -> eok x ())

  let bind p ~f =
    P
      (fun inp cok eok cerr eerr ->
        unparser p inp
          (fun x inp' () ->
            unparser (f x) inp' cok (fun x -> cok x inp') cerr (cerr inp'))
          (fun x () -> unparser (f x) inp cok eok cerr eerr)
          cerr eerr)

  let map =
    `Custom
      (fun p ~f ->
        P
          (fun inp cok eok cerr eerr ->
            unparser p inp
              (fun x inp' -> cok (f x) inp')
              (fun x -> eok (f x))
              cerr eerr))
end

module M = Monad.Make2 (Peasec_monad_basic)

module A = Applicative.Of_monad2 (struct
  type nonrec ('a, 'r) t = ('a, 'r) t

  include M
end)

include M
include A

let ( <|> ) lp rp =
  P
    (fun inp cok eok cerr eerr ->
      unparser lp inp cok eok cerr (fun () -> unparser rp inp cok eok cerr eerr))
