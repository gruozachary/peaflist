open! Base
open! Peasec
open! Let_syntax

let keywords = Set.of_list (module String) [ "fun"; "let"; "in"; "vd"; "td" ]

let id : Ast.id t =
  lexeme
    (trye
       (let%bind first = letter in
        let%bind rest = many (first_ok (first_ok letter digit) (char '_')) in
        let x = String.of_char_list (first :: rest) in
        if Set.mem keywords x then fail else return x))

let ty_var : Ast.ty_var t =
  lexeme
    (let%bind _ = char '\'' in
     let%map xs = many (first_ok (first_ok letter digit) (char '_')) in
     String.of_char_list ('\'' :: xs))

let ty_id : Ast.ty_id t = first_ok ty_var id

let keyword (s : string) : unit t =
  let%bind _ = string s in
  let%map _ = spaces_1 in
  ()

(* TODO: this will result on a horrible disaster if there are too many digits *)
and int : Ast.int t =
  lexeme
    (let%bind first = digit in
     let%map rest = many digit in
     Int.of_string (String.of_char_list (first :: rest)))

let rec expr () = choice [ binding (); lambda (); append () ]

and binding () =
  let%bind _ = trye (keyword "let") in
  let%bind x = id in
  let%bind _ = symbol "=" in
  let%bind e1 = expr () in
  let%bind _ = string "in" in
  let%bind _ = spaces_1 in
  let%map e2 = expr () in
  Ast.Binding (x, e1, e2)

and lambda () =
  let%bind _ = trye (keyword "fun") in
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
    (let%map x =
       first_ok
         (trye
            (let%bind x = symbol "+" in
             let%map _ = not_followed_by (symbol "+") in
             x))
         (symbol "-")
     in
     fun l r ->
       match x with
       | "+" -> Ast.BinOp (l, Ast.Plus, r)
       | "-" -> Ast.BinOp (l, Ast.Sub, r)
       | _ -> assert false)

and mul () =
  chain_left_1 (apply ())
    (let%map x = first_ok (symbol "*") (symbol "/") in
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
  chain_right_1 (ty_atom ())
    (let%map _ = symbol "*" in
     fun lt rt -> Ast.TyProd (lt, rt))

and ty_atom () =
  first_ok
    (let%map x = ty_id in
     Ast.TyId x)
    (let%bind _ = symbol "(" in
     let%bind t = ty () in
     let%map _ = symbol ")" in
     t)

let type_decl =
  let%bind _ = keyword "td" in
  let%bind x = id in
  let%bind _ = symbol ":=" in
  let%map ts =
    some
      (let%bind _ = symbol "|" in
       let%bind y = id in
       let%map t =
         option ~def:None
           (let%map t = ty () in
            Some t)
       in
       (y, t))
  in
  Ast.TypeDecl (x, ts)

let prog =
  fully
    (let%bind vd = many (first_ok val_decl type_decl) in
     let%map _ = spaces in
     vd)
