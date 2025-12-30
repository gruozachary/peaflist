open! Base

type t =
  { mutable next_tv : Type_var.t
  ; ident_renamer_heart : Ident.t Renamer.heart
  ; type_ident_renamer_heart : Type_ident.t Renamer.heart
  }

let create () =
  { next_tv = Type_var.zero
  ; ident_renamer_heart = Renamer.fresh_heart (module Ident)
  ; type_ident_renamer_heart = Renamer.fresh_heart (module Type_ident)
  }
;;

let fresh_tv s =
  let v = s.next_tv in
  s.next_tv <- Type_var.succ s.next_tv;
  v
;;

let fresh s = Type.TVar (fresh_tv s)
let ident_renamer_heart s = s.ident_renamer_heart
let type_ident_renamer_heart s = s.type_ident_renamer_heart
