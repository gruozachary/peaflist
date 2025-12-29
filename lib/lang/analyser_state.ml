open! Base

type t =
  { mutable next_tv : Type_var.t
  ; mutable next_ident : Core.Ident.t
  }

let create () = { next_tv = Type_var.zero; next_ident = Core.Ident.zero }

let fresh_tv s =
  let v = s.next_tv in
  s.next_tv <- Type_var.succ s.next_tv;
  v
;;

let fresh s = Type.TVar (fresh_tv s)

let next_ident s =
  let v = s.next_ident in
  s.next_ident <- Core.Ident.succ s.next_ident;
  v
;;
