open! Base
open Sqc

module Line : sig
  module CommandKind : sig
    type t =
      | Quit
      | Help
      | TypeOf of Lang.Parsed_ast.Expr.t
      | TypeInfo of string

    type meta =
      { _name : string
      ; _description : string
      ; cmd : string
      ; parser : t Peasec.t
      }

    val metas : meta list
  end

  type t =
    | Expr of Lang.Parsed_ast.Expr.t
    | Decl of Lang.Parsed_ast.Decl.t
    | Command of CommandKind.t

  val parse : string -> t Option.t
end = struct
  module CommandKind = struct
    type t =
      | Quit
      | Help
      | TypeOf of Lang.Parsed_ast.Expr.t
      | TypeInfo of string

    type meta =
      { _name : string
      ; _description : string
      ; cmd : string
      ; parser : t Peasec.t
      }

    let metas =
      let open Peasec in
      [ { _name = "Quit"
        ; _description = "Quits the toplevel."
        ; cmd = "q"
        ; parser = return Quit
        }
      ; { _name = "Help"
        ; _description = "Shows the help command."
        ; cmd = "h"
        ; parser = return Help
        }
      ; { _name = "Value type information"
        ; _description = "Gets the type information about a variable."
        ; cmd = "t"
        ; parser = (Parser.expr () >>| fun e -> TypeOf e)
        }
      ; { _name = "Type definition information"
        ; _description = "Get the information about a type definition."
        ; cmd = "i"
        ; parser = (Parser.Ident.lower >>| fun x -> TypeInfo x)
        }
      ]
    ;;

    let parser =
      let open Peasec in
      char ':'
      >*> (List.map metas ~f:(fun { cmd; parser; _ } -> lexeme (string cmd) >*> parser)
           |> choice)
    ;;
  end

  type t =
    | Expr of Lang.Parsed_ast.Expr.t
    | Decl of Lang.Parsed_ast.Decl.t
    | Command of CommandKind.t

  let parser =
    let open Peasec in
    let expr = Parser.expr () >>| fun e -> Expr e
    and command = CommandKind.parser >>| fun c -> Command c
    and val_decl = Parser.val_decl >>| fun d -> Decl d
    and type_decl = Parser.type_decl >>| fun d -> Decl d in
    command <|> val_decl <|> type_decl <|> expr
  ;;

  let parse = Peasec.exec parser
end

type t =
  { rename_ctx : Lang.Rename.t
  ; typecheck_ctx : Lang.Typecheck.ctx
  }

type run_output =
  { ctx : t
  ; should_quit : bool
  }

let handle_command ctx cmd =
  let open Result in
  match cmd with
  | Line.CommandKind.Quit -> return { ctx; should_quit = true }
  | Line.CommandKind.Help ->
    List.fold
      Line.CommandKind.metas
      ~init:"Command help:"
      ~f:(fun acc { _name; _description; cmd; _ } ->
        acc ^ "\n" ^ _name ^ ": " ^ _description ^ " (:" ^ cmd ^ ")")
    |> Stdio.print_endline;
    return { ctx; should_quit = false }
  | Line.CommandKind.TypeOf _ ->
    Stdio.print_endline "TODO: RE-IMPLEMENT";
    return { ctx; should_quit = false }
  | Line.CommandKind.TypeInfo _ ->
    Stdio.print_endline "TODO: RE-IMPLEMENT";
    return { ctx; should_quit = false }
;;

let run ctx =
  let open Result in
  let open Result.Let_syntax in
  let%bind line =
    Stdio.print_string "> ";
    Stdio.Out_channel.flush Stdio.stdout;
    Stdio.In_channel.input_line ~fix_win_eol:true Stdio.stdin
    |> of_option ~error:"Failed to take input."
  in
  match%bind Line.parse line |> of_option ~error:"Parse of input failed." with
  | Line.Expr expr ->
    let%bind expr = Lang.Rename.rename_expr ctx.rename_ctx expr in
    let expr = Lang.Desugar.desugar_expr ctx.rename_ctx expr in
    let%map _ = Lang.Typecheck.typecheck_expr ctx.typecheck_ctx ctx.rename_ctx expr in
    { ctx; should_quit = false }
  | Line.Decl decl ->
    let%bind decl, rename_ctx = Lang.Rename.rename_decl ctx.rename_ctx decl in
    let decl = Lang.Desugar.desugar_decl ctx.rename_ctx decl in
    let%map _, typecheck_ctx =
      Lang.Typecheck.typecheck_decl ctx.typecheck_ctx ctx.rename_ctx decl
    in
    { ctx = { typecheck_ctx; rename_ctx }; should_quit = false }
  | Line.Command cmd -> handle_command ctx cmd
;;

let loop () =
  let rename_ctx = Lang.Rename.basic () in
  let typecheck_ctx = Lang.Typecheck.basic rename_ctx in
  let ctx = { rename_ctx; typecheck_ctx } in
  let rec go ctx =
    match run ctx with
    | Result.Ok { should_quit = true; _ } -> ()
    | Result.Ok { ctx; _ } -> go ctx
    | Result.Error msg ->
      Stdio.print_endline ("Error: " ^ msg);
      go ctx
  in
  go ctx
;;
