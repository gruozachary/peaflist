open! Base

type t =
  { map : (string, Ident.t, String.comparator_witness) Map.t
  ; mutable next_ident : Ident.t
  }

let empty () = { map = Map.empty (module String); next_ident = Ident.zero }

let declare r ~str =
  let i = r.next_ident in
  r.next_ident <- Ident.succ r.next_ident;
  { r with map = Map.set r.map ~key:str ~data:i }
;;

(* val fetch : t -> str:string -> Ident.t Option.t *)
let fetch r ~str = Map.find r.map str
