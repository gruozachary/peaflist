open! Base

type t = (string, Ident.t, String.comparator_witness) Map.t
type heart = { mutable next_ident : Ident.t }

let fresh_heart () = { next_ident = Ident.zero }
let empty = Map.empty (module String)

let declare r ~heart ~str =
  let i = heart.next_ident in
  heart.next_ident <- Ident.succ heart.next_ident;
  Map.set r ~key:str ~data:i
;;

let fetch r ~str = Map.find r str

let declare_and_fetch r ~heart ~str =
  let r = declare r ~heart ~str in
  Map.find_exn r str, r
;;

let merge r_1 r_2 =
  Map.merge r_1 r_2 ~f:(fun ~key:_ -> function
    | `Left x -> Option.Some x
    | `Right x -> Option.Some x
    | `Both _ -> assert false)
;;
