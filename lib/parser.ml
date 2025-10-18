open! Base
open Peasec
open Let_syntax

let keywords =
  Set.of_list (module String) [ "fun"; "let"; "in"; "vd"; "td"; "of"; "with" ]

let symbols =
  [
    ":=";
    "(";
    ")";
    ",";
    "|";
    "->";
    "*";
    "=";
    "{";
    "}";
    "[";
    "]";
    "+";
    "-";
    "/";
    "++";
    "'";
  ]

let symbol_trie =
  match
    Trie.of_alist Trie.Of_string.Keychain.keychainable
      (List.map symbols ~f:(fun s -> (s, ())))
  with
  | Ok s -> s
  | _ -> assert false

(* TODO: work out what's going on with the empty string stuff *)
let symbol s =
  let go p s =
    Trie.foldi (Trie.find_trie symbol_trie s) ~init:p
      ~f:(fun acc ~keychain ~data:_ ->
        if not (String.is_empty keychain) then
          acc <*< not_followed_by (string keychain)
        else acc)
  in
  lexeme (attempt (go (string s) s))

let id_char = letter <|> digit <|> char '_'

let id : Ast.id t =
  lexeme
    (attempt
       (let%bind first = letter in
        let%bind rest = many id_char in
        let x = String.of_char_list (first :: rest) in
        if Set.mem keywords x then fail else return x))

let ty_var : Ast.ty_var t =
  lexeme (char '\'' >>| List.cons <*> many id_char >>| String.of_char_list)

let ty_id : Ast.ty_id t = ty_var <|> id

let keyword (s : string) : unit t = lexeme (string s >*> not_followed_by id_char)

(* TODO: this will result on a horrible disaster if there are too many digits *)
and int : Ast.int t =
  lexeme (some digit >>| String.of_char_list >>| Int.of_string)

let rec expr () = choice [ binding (); lambda (); append () ]

and binding () =
  let%bind _ = attempt (keyword "let") in
  let%bind x = id in
  let%bind _ = symbol "=" in
  let%bind e1 = expr () in
  let%bind _ = keyword "in" in
  let%map e2 = expr () in
  Ast.Binding (x, e1, e2)

and lambda () =
  let%bind _ = attempt (keyword "fun") in
  let%bind x = id in
  let%bind _ = symbol "->" in
  let%map e = expr () in
  Ast.Lambda (x, e)

and append () =
  chain_right_1 (add ())
    (let%map _ = symbol "++" in
     fun l r -> Ast.BinOp (l, Ast.Append, r))

and add () =
  chain_left_1 (mul ())
    (let%map x = symbol "+" <|> symbol "-" in
     fun l r ->
       match x with
       | "+" -> Ast.BinOp (l, Ast.Plus, r)
       | "-" -> Ast.BinOp (l, Ast.Sub, r)
       | _ -> assert false)

and mul () =
  chain_left_1 (apply ())
    (let%map x = symbol "*" <|> symbol "/" in
     fun l r ->
       match x with
       | "*" -> Ast.BinOp (l, Ast.Mul, r)
       | "/" -> Ast.BinOp (l, Ast.Div, r)
       | _ -> assert false)

and apply () =
  let%bind x = atom () in
  let%map xs = many (atom ()) in
  List.fold ~init:x ~f:(fun f x -> Ast.Apply (f, x)) xs

and atom () =
  choice
    [
      (let%map x = int in
       Ast.Int x);
      (let%map x = id in
       Ast.Id x);
      (let%map e = between ~l:(symbol "(") ~r:(symbol ")") (defer expr) in
       Ast.Group e);
      (let%map es =
         between ~l:(symbol "[") ~r:(symbol "]")
           (sep_by_1 ~sep:(symbol ",") (defer expr))
       in
       Ast.List es);
      (let%map es =
         between ~l:(symbol "{") ~r:(symbol "}")
           (sep_by_1 ~sep:(symbol ",") (defer expr))
       in
       Ast.Tuple es);
    ]

let val_decl =
  let%bind _ = keyword "vd" in
  let%bind x = id in
  let%bind _ = symbol ":=" in
  let%map e = expr () in
  Ast.ValDecl (x, e)

let rec ty () = ty_fun ()

and ty_fun () =
  chain_right_1 (ty_prod ())
    (let%map _ = symbol "->" in
     fun lt rt -> Ast.TyFun (lt, rt))

and ty_prod () =
  chain_right_1 (ty_app ())
    (let%map _ = symbol "*" in
     fun lt rt -> Ast.TyProd (lt, rt))

and ty_app () =
  let%bind x = ty_atom () in
  let%map xs = many (ty_atom ()) in
  List.fold ~init:x ~f:(fun lt rt -> Ast.TyApp (lt, rt)) xs

and ty_atom () =
  (let%map x = ty_id in
   Ast.TyId x)
  <|> between ~l:(symbol "(") (defer ty) ~r:(symbol ")")

let type_decl =
  let%bind _ = keyword "td" in
  let%bind tvs =
    between ~l:(symbol "(") ~r:(symbol ")") (sep_by ~sep:(symbol ",") ty_var)
  in
  let%bind x = id in
  let%bind _ = symbol ":=" in
  let%map ts =
    some
      (let%bind _ = symbol "|" in
       let%bind y = id in
       let%map t =
         option ~def:None (keyword "of" >*> ty () >>| fun t -> Some t)
       in
       (y, t))
  in
  Ast.TypeDecl (x, tvs, ts)

let prog =
  fully
    (let%bind vd = many (val_decl <|> type_decl) in
     let%map _ = spaces in
     vd)
