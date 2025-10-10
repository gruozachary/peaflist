open! Base

type 'a t = {
  run :
    'r.
    string -> ('a -> string -> 'r) -> ('a -> 'r) -> (string -> 'r) -> 'r -> 'r;
}

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

module Peasec_applicative_basic = struct
  type nonrec 'a t = 'a t

  let return x = { run = (fun _ _ eok _ _ -> eok x) }

  let apply (af : ('a -> 'b) t) (p : 'a t) : 'b t =
    {
      run =
        (fun inp cok eok cerr eerr ->
          let cok' f inp' =
            p.run inp'
              (fun x -> cok (f x))
              (fun x -> cok (f x) inp')
              cerr (cerr inp')
          in
          let eok' f =
            p.run inp (fun x -> cok (f x)) (fun x -> eok (f x)) cerr eerr
          in
          af.run inp cok' eok' cerr eerr);
    }

  let map = `Define_using_apply
end

include Applicative.Make (Peasec_applicative_basic)
