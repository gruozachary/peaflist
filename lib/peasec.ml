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

  let map = `Define_using_bind

  module Let_syntax = struct
    module Let_syntax = struct
      let return = return
      let bind = bind
      let map = map
      let both mx my = bind mx ~f:(fun x -> bind my ~f:(fun y -> return (x, y)))
      let _ = (return, bind, map, both)
    end
  end
end

include Monad.Make (Peasec_monad_basic)
