open! Base
open Sqc
open Typechecking

module Line : sig
  module CommandKind : sig
    type t =
      | Quit
      | Help
      | TypeOf of (string, Ast.Expr.t) Either.t
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
    | Expr of Ast.Expr.t
    | Decl of Ast.decl
    | Command of CommandKind.t

  val parse : string -> t Option.t
end = struct
  module CommandKind = struct
    type t =
      | Quit
      | Help
      | TypeOf of (string, Ast.Expr.t) Either.t
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
        ; parser =
            (attempt (Parser.Ident.lower <*< eof >>| fun x -> Either.First x)
             <|> (Parser.expr () >>| fun e -> Either.Second e)
             >>| fun e -> TypeOf e)
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
    | Expr of Ast.Expr.t
    | Decl of Ast.decl
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

type t = { mutable semantic_ctx : Ctx.t }

let handle_command toplevel_ctx cmd =
  let open Result in
  let open Result.Let_syntax in
  match cmd with
  | Line.CommandKind.Quit -> Ok true
  | Line.CommandKind.Help ->
    List.fold
      Line.CommandKind.metas
      ~init:"Command help:"
      ~f:(fun acc { _name; _description; cmd; _ } ->
        acc ^ "\n" ^ _name ^ ": " ^ _description ^ " (:" ^ cmd ^ ")")
    |> Stdio.print_endline;
    Ok false
  | Line.CommandKind.TypeOf (Either.First x) ->
    let%map scheme =
      Ctx.Env.get toplevel_ctx.semantic_ctx
      |> Gamma.lookup ~id:x
      |> of_option ~error:"Unbound variable identifier"
    in
    Stdio.print_endline (Scheme.to_string scheme);
    false
  | Line.CommandKind.TypeOf (Either.Second e) ->
    let%map ty = typecheck_expr toplevel_ctx.semantic_ctx e in
    Stdio.print_endline (Tau.to_string ty);
    false
  | Line.CommandKind.TypeInfo x ->
    let%map arity =
      Ctx.Tenv.get toplevel_ctx.semantic_ctx
      |> TyEnv.lookup ~id:x
      |> of_option ~error:"Unbound type identifier"
    in
    Stdio.print_endline ("Arity: " ^ Int.to_string arity);
    false
;;

let run toplevel_ctx =
  let open Result in
  let open Result.Let_syntax in
  let%bind line =
    Stdio.print_string "> ";
    Stdio.Out_channel.flush Stdio.stdout;
    Stdio.In_channel.input_line ~fix_win_eol:true Stdio.stdin
    |> of_option ~error:"Failed to take input."
  in
  match%bind Line.parse line |> of_option ~error:"Parse of input failed." with
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
  | Line.Command cmd -> handle_command toplevel_ctx cmd
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
