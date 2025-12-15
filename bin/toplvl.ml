open! Base
open Sqc
open Typechecking

module Line = struct
  module CommandKind = struct
    type t =
      | Quit
      | TypeOf of string
  end

  type t =
    | Expr of Ast.Expr.t
    | Command of CommandKind.t
end

module TLParser : sig
  val parse_string : string -> Line.t Option.t
end = struct
  open Peasec

  let command =
    char ':'
    >*> choice [ lexeme (char 'q') >*> return Line.CommandKind.Quit ]
    >>| fun ck -> Line.Command ck
  ;;

  let parse () = command <|> (Parser.expr () >>| fun e -> Line.Expr e)
  let parse_string = exec (parse () <*< eof)
end

type t = { mutable env : Ctx.t }

let run tl =
  let open Result in
  let open Result.Let_syntax in
  let%bind line =
    Stdio.print_string "> ";
    Stdio.Out_channel.flush Stdio.stdout;
    Stdio.In_channel.input_line ~fix_win_eol:true Stdio.stdin
    |> of_option ~error:"Failed to take input."
  in
  match%bind TLParser.parse_string line |> of_option ~error:"Parse of input failed." with
  | Line.Expr e ->
    let%map ty = typecheck_expr tl.env e in
    Stdio.print_endline (Tau.to_string ty);
    false
  | Line.Command Line.CommandKind.Quit -> Ok true
  | _ -> Error "Not implemented"
;;

let rec loop () =
  let tl = { env = Ctx.empty () } in
  match run tl with
  | Result.Ok true -> ()
  | Result.Ok false -> loop ()
  | Result.Error msg ->
    Stdio.print_endline ("Error: " ^ msg);
    loop ()
;;
