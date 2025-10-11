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
            (fun x inp' -> (f x).run inp' cok eok cerr eerr)
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

let rec many (p : 'a t) : 'a list t =
  first_ok
    ( p >>= fun x ->
      many p >>| fun xs -> x :: xs )
    (return [])
