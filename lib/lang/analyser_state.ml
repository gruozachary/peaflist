open! Base

type t =
  { mutable next_tv : Type_var.t
  ; renamer_heart : Renamer.heart
  }

let create () = { next_tv = Type_var.zero; renamer_heart = Renamer.fresh_heart () }

let fresh_tv s =
  let v = s.next_tv in
  s.next_tv <- Type_var.succ s.next_tv;
  v
;;

let fresh s = Type.TVar (fresh_tv s)
let renamer_heart s = s.renamer_heart
