open! Base

type t = { mutable next : int }

let create () = { next = 0 }

let fresh_tv s =
  let v = s.next in
  s.next <- v + 1;
  Tvar.of_int v
;;

let fresh s = Tau.TVar (fresh_tv s)
