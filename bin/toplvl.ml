open! Base
open Sqc
open Typechecking

module Parser : sig
  val parse_string : string -> Ast.Expr.t Option.t
end = struct
  open Peasec

  let parse () = Parser.expr ()
  let parse_string = exec (parse ())
end

type t = { mutable env : Ctx.t }

let run tl =
  let open Result.Let_syntax in
  let%bind line =
    Stdio.print_string "> ";
    Stdio.In_channel.input_line ~fix_win_eol:true Stdio.stdin
    |> Result.of_option ~error:"Failed to take input."
  in
  let%bind parsed_line =
    Parser.parse_string line |> Result.of_option ~error:"Parse of input failed."
  in
  let%map ty = typecheck_expr tl.env parsed_line in
  Stdio.print_endline (Tau.to_string ty)
;;

let rec loop () =
  let tl = { env = Ctx.empty () } in
  (match run tl with
   | Result.Ok () -> ()
   | Result.Error msg -> Stdio.print_endline ("Error: " ^ msg));
  loop ()
;;
