open! Base
open Sqc
open Typechecking

module Line = struct
  module CommandKind = struct
    type t =
      | Quit
      | TypeOf of (string, Ast.Expr.t) Either.t
      | TypeInfo of string
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
    and type_of =
      attempt (Parser.Ident.lower <*< eof >>| fun x -> Either.First x)
      <|> (Parser.expr () >>| fun e -> Either.Second e)
      >>| fun e -> Line.CommandKind.TypeOf e
    and type_info = Parser.Ident.lower >>| fun x -> Line.CommandKind.TypeInfo x in
    char ':'
    >*> choice
          [ lexeme (char 'q') >*> quit
          ; lexeme (char 't') >*> type_of
          ; lexeme (char 'i') >*> type_info
          ]
    >>| fun ck -> Line.Command ck
  ;;

  let expr = Parser.expr () >>| fun e -> Line.Expr e
  let val_decl = Parser.val_decl >>| fun d -> Line.Decl d
  let type_decl = Parser.type_decl >>| fun d -> Line.Decl d
  let parse () = command <|> val_decl <|> type_decl <|> expr
  let parse_string = exec (parse () <*< eof)
end

type t = { mutable semantic_ctx : Ctx.t }

let run toplevel_ctx =
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
    let%map _ = typecheck_expr toplevel_ctx.semantic_ctx e in
    false
  | Line.Decl (Ast.ValDecl (x, e)) ->
    let%map ctx = typecheck_val_decl toplevel_ctx.semantic_ctx x e in
    toplevel_ctx.semantic_ctx <- ctx;
    false
  | Line.Decl (Ast.TypeDecl (x, utvs, ctors)) ->
    let%map ctx = typecheck_type_decl toplevel_ctx.semantic_ctx x utvs ctors in
    toplevel_ctx.semantic_ctx <- ctx;
    false
  | Line.Command Line.CommandKind.Quit -> Ok true
  | Line.Command (Line.CommandKind.TypeOf (Either.First x)) ->
    let%map scheme =
      Ctx.Env.get toplevel_ctx.semantic_ctx
      |> Gamma.lookup ~id:x
      |> of_option ~error:"Unbound variable identifier"
    in
    Stdio.print_endline (Scheme.to_string scheme);
    false
  | Line.Command (Line.CommandKind.TypeOf (Either.Second e)) ->
    let%map ty = typecheck_expr toplevel_ctx.semantic_ctx e in
    Stdio.print_endline (Tau.to_string ty);
    false
  | Line.Command (Line.CommandKind.TypeInfo x) ->
    let%map arity =
      Ctx.Tenv.get toplevel_ctx.semantic_ctx
      |> TyEnv.lookup ~id:x
      |> of_option ~error:"Unbound type identifier"
    in
    Stdio.print_endline ("Arity: " ^ Int.to_string arity);
    false
;;

let loop () =
  let toplevel_ctx = { semantic_ctx = Ctx.empty () } in
  let rec go () =
    match run toplevel_ctx with
    | Result.Ok true -> ()
    | Result.Ok false -> go ()
    | Result.Error msg ->
      Stdio.print_endline ("Error: " ^ msg);
      go ()
  in
  go ()
;;
