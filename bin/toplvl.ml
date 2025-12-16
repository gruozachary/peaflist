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
    | Decl of Ast.decl
    | Command of CommandKind.t
end

module TLParser : sig
  val parse_string : string -> Line.t Option.t
end = struct
  open Peasec

  let command =
    let quit = return Line.CommandKind.Quit
    and type_of = Parser.Ident.lower >>| fun x -> Line.CommandKind.TypeOf x in
    char ':'
    >*> choice [ lexeme (char 'q') >*> quit; lexeme (char 't') >*> type_of ]
    >>| fun ck -> Line.Command ck
  ;;

  let expr = Parser.expr () >>| fun e -> Line.Expr e
  let val_decl = Parser.val_decl >>| fun d -> Line.Decl d
  let parse () = command <|> val_decl <|> expr
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
  | Line.Decl (Ast.ValDecl (x, e)) ->
    let%map ctx = typecheck_val_decl tl.env x e in
    tl.env <- ctx;
    false
  | Line.Decl (Ast.TypeDecl _) -> Error "TODO: Implement"
  | Line.Command Line.CommandKind.Quit -> Ok true
  | Line.Command (Line.CommandKind.TypeOf x) ->
    let%map scheme =
      Ctx.Env.get tl.env |> Gamma.lookup ~id:x |> of_option ~error:"Unbound identifier"
    in
    Stdio.print_endline (Scheme.to_string scheme);
    false
;;

let loop () =
  let tl = { env = Ctx.empty () } in
  let rec go () =
    match run tl with
    | Result.Ok true -> ()
    | Result.Ok false -> go ()
    | Result.Error msg ->
      Stdio.print_endline ("Error: " ^ msg);
      go ()
  in
  go ()
;;
