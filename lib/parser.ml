open! Base
open Peasec
open Let_syntax
open Ast

let keywords =
  Set.of_list (module String) [ "fun"; "let"; "in"; "vd"; "td"; "of"; "with" ]
;;

let symbols =
  [ ":="; "("; ")"; ","; "|"; "->"; "*"; "="; "{"; "}"; "["; "]"; "+"; "-"; "/"; "'" ]
;;

let symbol_trie =
  match
    Trie.of_alist
      Trie.Of_string.Keychain.keychainable
      (List.map symbols ~f:(fun s -> s, ()))
  with
  | Ok s -> s
  | _ -> assert false
;;

(* TODO: work out what's going on with the empty string stuff *)
let symbol s =
  let go p s =
    Trie.foldi (Trie.find_trie symbol_trie s) ~init:p ~f:(fun acc ~keychain ~data:_ ->
      if not (String.is_empty keychain)
      then acc <*< not_followed_by (string keychain)
      else acc)
  in
  lexeme (attempt (go (string s) s))
;;

module Ident : sig
  val lower : string t
  val upper : string t
  val dash : string t
  val ty : string t
  val char : char t
end = struct
  let char = letter <|> digit <|> Peasec.char '_'

  let mk_id_parser p =
    lexeme
      (attempt
         (let%bind first = p in
          let%bind rest = many char in
          let x = first ^ String.of_char_list rest in
          if Set.mem keywords x then fail else return x))
  ;;

  let lower = mk_id_parser (lowercase >>| String.of_char)
  let upper = mk_id_parser (uppercase >>| String.of_char)

  let dash =
    mk_id_parser
      (let%map first = Peasec.char '\'' >*> letter in
       "\'" ^ String.of_char first)
  ;;

  let ty = dash <|> lower
end

let keyword (s : string) : unit t = lexeme (string s >*> not_followed_by Ident.char)

(* TODO: this will result on a horrible disaster if there are too many digits *)
let int : Ast.int t = lexeme (some digit >>| String.of_char_list >>| Int.of_string)
let tuple p = between ~l:(symbol "(") ~r:(symbol ")") (sep_by ~sep:(symbol ",") p)
let tuple_1 p = between ~l:(symbol "(") ~r:(symbol ")") (sep_by_1 ~sep:(symbol ",") p)

module Pattern : sig
  val parse : unit -> Pat.t t
end = struct
  let rec parse () =
    choice
      [ (let%map x = int in
         Pat.Int x)
      ; (let%map x = Ident.lower in
         Pat.Ident x)
      ; (let%bind x = Ident.upper in
         let%map p_opt = option_opt (defer parse) in
         Pat.CtorApp (x, p_opt))
      ; (let%map ps = tuple_1 (defer parse) in
         Pat.Tuple ps)
      ]
  ;;
end

module Expression : sig
  val parse : unit -> Expr.t t
end = struct
  let rec parse () = choice [ binding (); lambda (); matching (); add () ]

  and binding () =
    let%bind _ = attempt (keyword "let") in
    let%bind x = Ident.lower in
    let%bind _ = symbol "=" in
    let%bind e1 = parse () in
    let%bind _ = keyword "in" in
    let%map e2 = parse () in
    Expr.Binding (x, e1, e2)

  and lambda () =
    let%bind _ = attempt (keyword "fun") in
    let%bind x = Ident.lower in
    let%bind _ = symbol "->" in
    let%map e = parse () in
    Expr.Lambda (x, e)

  and matching () =
    let%bind _ = attempt (keyword "match") in
    let%bind e = parse () in
    let%bind _ = keyword "with" in
    let%map es =
      some
        (let%bind p = symbol "|" >*> Pattern.parse () in
         let%map e' = symbol "->" >*> parse () in
         p, e')
    in
    Expr.Match (e, es)

  and add () =
    chain_left_1
      (mul ())
      (let%map x = symbol "+" <|> symbol "-" in
       fun l r ->
         match x with
         | "+" -> Expr.BinOp (l, Expr.Bin_op.Plus, r)
         | "-" -> Expr.BinOp (l, Expr.Bin_op.Sub, r)
         | _ -> assert false)

  and mul () =
    chain_left_1
      (apply ())
      (let%map x = symbol "*" <|> symbol "/" in
       fun l r ->
         match x with
         | "*" -> Expr.BinOp (l, Expr.Bin_op.Mul, r)
         | "/" -> Expr.BinOp (l, Expr.Bin_op.Div, r)
         | _ -> assert false)

  and apply () =
    let%bind x = atom () in
    let%map xs = many (atom ()) in
    List.fold ~init:x ~f:(fun f x -> Expr.Apply (f, x)) xs

  and atom () =
    choice
      [ (let%map x = int in
         Expr.Int x)
      ; (let%map x = Ident.lower in
         Expr.Id x)
      ; (let%map x = Ident.upper in
         Expr.Constr x)
      ; (match%map tuple_1 (defer parse) with
         | [ e ] -> Expr.Group e
         | es -> Expr.Tuple es)
      ]
  ;;
end

module Type : sig
  val parse : unit -> Ty.t t
end = struct
  let rec parse () = ty_fun ()

  and ty_fun () =
    chain_right_1
      (ty_prod ())
      (let%map _ = symbol "->" in
       fun lt rt -> Ty.Fun (lt, rt))

  and ty_prod () =
    match%map sep_by_1 ~sep:(symbol "*") (ty_app ()) with
    | [ x ] -> x
    | xs -> Ty.Prod xs

  and ty_app () =
    let%bind head =
      (match%bind tuple_1 (defer parse) with
       | [ x ] -> return x
       | xs ->
         let%map tid = Ident.ty in
         Ty.App (tid, xs))
      <|>
      let%map x = Ident.ty in
      Ty.Id x
    in
    let%map rest = many Ident.ty in
    List.fold ~init:head ~f:(fun acc t -> Ty.App (t, [ acc ])) rest
  ;;
end

let val_decl =
  let%bind _ = keyword "vd" in
  let%bind x = Ident.lower in
  let%bind _ = symbol ":=" in
  let%map e = Expression.parse () in
  ValDecl (x, e)
;;

let type_decl =
  let%bind _ = keyword "td" in
  let%bind tvs = tuple Ident.dash in
  let%bind x = Ident.lower in
  let%bind _ = symbol ":=" in
  let%map ts =
    some
      (let%bind _ = symbol "|" in
       let%bind y = Ident.upper in
       let%map t =
         option ~def:None (keyword "of" >*> Type.parse () >>| fun t -> Some t)
       in
       y, t)
  in
  Ast.TypeDecl (x, tvs, ts)
;;

let prog =
  fully
    (let%bind vd = many (val_decl <|> type_decl) in
     let%map _ = spaces in
     vd)
;;
