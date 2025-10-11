open! Base

let parser : string Sqc.Peasec.t =
  let open Parser in
  let%map _ = symbol (char '(') and x = symbol id and _ = symbol (char ')') in
  x

let () =
  let open Stdio in
  match In_channel.input_line In_channel.stdin with
  | Some line -> (
      match Sqc.Peasec.exec parser line with
      | Some s -> print_endline s
      | _ -> ())
  | _ -> ()
