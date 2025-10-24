open! Base

type 'a t =
  { unparser :
      'r.
      string
      -> ('a -> string -> unit -> 'r)
      -> ('a -> unit -> 'r)
      -> (string -> unit -> 'r)
      -> (unit -> 'r)
      -> 'r
  }

(* 'a - What this parser gives to it's next continuations *)
(* 'r - What the next continuation returns *)

let satisfy (f : char -> bool) : char t =
  { unparser =
      (fun inp cok _ _ eerr ->
        if String.length inp > 0 && f inp.[0]
        then cok inp.[0] (String.drop_prefix inp 1) ()
        else eerr ())
  }
;;

let exec p inp =
  p.unparser
    inp
    (fun x _ () -> Option.Some x)
    (fun x () -> Option.Some x)
    (fun _ () -> Option.None)
    (fun () -> Option.None)
;;

module Peasec_monad_basic = struct
  type nonrec 'a t = 'a t

  let return x = { unparser = (fun _ _ eok _ _ -> eok x ()) }

  let bind p ~f =
    { unparser =
        (fun inp cok eok cerr eerr ->
          p.unparser
            inp
            (fun x inp' () ->
               (f x).unparser inp' cok (fun x -> cok x inp') cerr (cerr inp'))
            (fun x () -> (f x).unparser inp cok eok cerr eerr)
            cerr
            eerr)
    }
  ;;

  let map =
    `Custom
      (fun p ~f ->
        { unparser =
            (fun inp cok eok cerr eerr ->
              p.unparser inp (fun x inp' -> cok (f x) inp') (fun x -> eok (f x)) cerr eerr)
        })
  ;;
end

module M = Monad.Make (Peasec_monad_basic)

module A = Applicative.Of_monad (struct
    type nonrec 'a t = 'a t

    include M
  end)

include M
include A
open Let_syntax

(* classic applicative *)

let ( >*> ) lp rp = ignore_m lp *> rp
let ( <*< ) lp rp = lp <* ignore_m rp

(* alternative *)
let empty = { unparser = (fun _ _ _ _ eerr -> eerr ()) }

let ( <|> ) lp rp =
  { unparser =
      (fun inp cok eok cerr eerr ->
        lp.unparser inp cok eok cerr (fun () -> rp.unparser inp cok eok cerr eerr))
  }
;;

let choice = List.fold ~init:empty ~f:( <|> )

let many_acc (p : 'a t) (acc : 'a -> 'a list -> 'a list) : 'a list t =
  { unparser =
      (fun inp cok eok cerr eerr ->
        let rec walk xs x inp' () =
          p.unparser
            inp'
            (fun y -> walk (acc x xs) y)
            (fun _ -> eerr)
            cerr
            (cok (acc x xs) inp')
        in
        p.unparser inp (walk []) (fun _ -> eerr) cerr (eok []))
  }
;;

(* error manipulation *)

let attempt (p : 'a t) : 'a t =
  { unparser = (fun inp cok eok _ eerr -> p.unparser inp cok eok (fun _ -> eerr) eerr) }
;;

let fail : 'a t = { unparser = (fun _ _ _ _ eerr -> eerr ()) }

(* lookahead *)

let not_followed_by p =
  { unparser =
      (fun inp _ eok _ eerr ->
        p.unparser inp (fun _ _ -> eerr) (fun _ -> eerr) (fun _ -> eok ()) (eok ()))
  }
;;

let followed_by p =
  { unparser =
      (fun inp _ eok _ eerr ->
        p.unparser inp (fun _ _ -> eok ()) (fun _ -> eok ()) (fun _ -> eerr) eerr)
  }
;;

(* multi-parsers *)

let many (p : 'a t) : 'a list t = many_acc p List.cons >>| List.rev
let some (p : 'a t) : 'a list t = p >>| List.cons <*> many p

let chain_left_1 p op =
  let rec rest x =
    (let%bind f = op in
     let%bind y = p in
     rest (f x y))
    <|> return x
  in
  p >>= rest
;;

let chain_right_1 p op =
  let rec rest x =
    (let%bind f = op in
     let%map r = p >>= rest in
     f x r)
    <|> return x
  in
  p >>= rest
;;

let sep_by_1 p ~sep =
  let%bind x = p in
  let%bind xs = many (sep >*> p) in
  return (x :: xs)
;;

let sep_by p ~sep = sep_by_1 p ~sep <|> return []

(* misc *)

let between ~l p ~r = l >*> p <*< r

let option (p : 'a t) ~(def : 'a) : 'a t =
  { unparser =
      (fun inp cok eok _ _ -> p.unparser inp cok eok (fun _ -> eok def) (eok def))
  }
;;

let defer (f : unit -> 'a t) : 'a t =
  { unparser = (fun inp cok eok cerr eerr -> (f ()).unparser inp cok eok cerr eerr) }
;;

(*
   combinators which are more string-related
*)

(* character stuff *)

let char c = satisfy (equal_char c)
let letter = satisfy Char.is_alpha
let digit = satisfy Char.is_digit

(* whitespace stuff *)

let space = satisfy Char.is_whitespace
let spaces = ignore_m (many space)
let spaces_1 = space >*> spaces

(* string stuff *)

let string s =
  let rec string_check i =
    if i < String.length s
    then (
      let%bind _ = char s.[i] in
      string_check (i + 1))
    else return s
  in
  string_check 0
;;

let lexeme p = p <*< spaces

(* eof stuff *)

let eof =
  { unparser =
      (fun inp _ eok _ eerr ->
        if String.for_all ~f:Char.is_whitespace inp then eok () () else eerr ())
  }
;;

let fully p = spaces >*> p <*< eof
