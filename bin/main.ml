open! Base
open Parser

let () =
  let open Stdio in
  match In_channel.input_line In_channel.stdin with
  | Some line -> (
      match Sqc.Peasec.exec id line with Some s -> print_endline s | _ -> ())
  | _ -> ()
