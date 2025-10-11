open! Base

type 'a t = {
  run :
    'r.
    string -> ('a -> string -> 'r) -> ('a -> 'r) -> (string -> 'r) -> 'r -> 'r;
}

module Peasec_monad_basic = struct
  type nonrec 'a t = 'a t

  let return x = { run = (fun _ _ eok _ _ -> eok x) }

  let bind (p : 'a t) ~f : 'b t =
    {
      run =
        (fun inp cok eok cerr eerr ->
          p.run inp
            (fun x inp' ->
              (f x).run inp' cok (fun y -> cok y inp') cerr (cerr inp'))
            (fun x -> (f x).run inp cok eok cerr eerr)
            cerr eerr);
    }

  let map =
    `Custom
      (fun mx ~f ->
        {
          run =
            (fun inp cok eok cerr eerr ->
              mx.run inp (fun x -> cok (f x)) (fun x -> eok (f x)) cerr eerr);
        })
end

module M = Monad.Make (Peasec_monad_basic)

module A = Applicative.Of_monad (struct
  type nonrec 'a t = 'a t

  include M
end)

include M
include A

let exec (p : 'a t) (inp : string) : 'a Option.t =
  p.run inp
    (fun x _ -> Option.Some x)
    (fun x -> Option.Some x)
    (fun _ -> Option.None)
    Option.None

let satisfy (f : char -> bool) : char t =
  {
    run =
      (fun inp cok _ _ eerr ->
        if String.length inp > 0 && f inp.[0] then
          cok inp.[0] (String.drop_prefix inp 1)
        else eerr);
  }

let first_ok pl pr =
  {
    run =
      (fun inp cok eok cerr eerr ->
        pl.run inp cok eok cerr (pr.run inp cok eok cerr eerr));
  }

let many_acc (p : 'a t) (acc : 'a -> 'a list -> 'a list) : 'a list t =
  {
    run =
      (fun inp cok eok cerr eerr ->
        let rec walk xs x inp' =
          p.run inp'
            (fun y -> walk (acc x xs) y)
            (fun _ -> eerr)
            cerr
            (cok (acc x xs) inp')
        in
        p.run inp (walk []) (fun _ -> eerr) cerr (eok []));
  }

let many (p : 'a t) : 'a list t = map (many_acc p List.cons) ~f:List.rev

let atomic (p : 'a t) =
  { run = (fun inp cok eok _ eerr -> p.run inp cok eok (fun _ -> eerr) eerr) }

let eof =
  {
    run =
      (fun inp _ eok _ eerr ->
        if String.for_all ~f:Char.is_whitespace inp then eok () else eerr);
  }

open Let_syntax

let char c = satisfy (equal_char c)
let letter = satisfy Char.is_alpha
let digit = satisfy Char.is_digit
let space = satisfy Char.is_whitespace
let spaces = ignore_m (many space)

let string s =
  let rec string_check i =
    if i < String.length s then
      let%bind _ = char s.[i] in
      string_check (i + 1)
    else return s
  in
  string_check 0

let lexeme p =
  let%map x = p and _ = spaces in
  x

let fully p =
  let%map _ = spaces and x = p and _ = eof in
  x

let symbol s = lexeme (string s)
