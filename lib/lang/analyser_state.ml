open! Base

type t =
  { mutable next_tv : int
  ; mutable next_ident : Core.Ident.t
  }

let create () = { next_tv = 0; next_ident = Core.Ident.zero }

let fresh_tv s =
  let v = s.next_tv in
  s.next_tv <- v + 1;
  Type_var.of_int v
;;

let fresh s = Type.TVar (fresh_tv s)

let next_ident s =
  let v = s.next_ident in
  s.next_ident <- Core.Ident.succ s.next_ident;
  v
;;
