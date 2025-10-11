open! Base

let parser : string Sqc.Peasec.t =
  let open Parser in
  let%map _ = symbol "(" and x = lexeme id and _ = symbol ")" in
  x

let () =
  let open Stdio in
  match In_channel.input_line In_channel.stdin with
  | Some line -> (
      match Sqc.Peasec.exec (Parser.fully parser) line with
      | Some s -> print_endline ("Success: " ^ s)
      | _ -> print_endline "Failure")
  | _ -> ()
