open! Base

type heart = { mutable next_ident : Ident.t }

type t =
  { map : (string, Ident.t, String.comparator_witness) Map.t
  ; heart : heart
  }

let fresh_heart () = { next_ident = Ident.zero }
let empty heart = { map = Map.empty (module String); heart }

let declare r ~heart ~str =
  if not (phys_equal heart r.heart)
  then raise_s [%message "Only one heart can be used per renamer"];
  let i = heart.next_ident in
  heart.next_ident <- Ident.succ heart.next_ident;
  { r with map = Map.set r.map ~key:str ~data:i }
;;

let fetch r ~str = Map.find r.map str

let declare_and_fetch r ~heart ~str =
  let r = declare r ~heart ~str in
  Map.find_exn r.map str, r
;;

let merge r_1 r_2 =
  if not (phys_equal r_1.heart r_2.heart)
  then raise_s [%message "Cannot merge two renamers with different hearts"];
  { r_1 with
    map =
      Map.merge r_1.map r_2.map ~f:(fun ~key:_ -> function
        | `Left x -> Option.Some x
        | `Right x -> Option.Some x
        | `Both _ -> assert false)
  }
;;
