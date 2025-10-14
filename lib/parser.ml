open! Base
open! Peasec
open! Let_syntax

module AST = struct
  type nonrec int = int
  type id = string
  type bin_op = Plus | Sub | Mul | Div | Append

  type expr =
    | Int of int
    | Id of id
    | Apply of expr * expr
    | Group of expr
    | Lambda of id * expr
    | Binding of id * expr * expr
    | List of list_elems
    | BinOp of expr * bin_op * expr

  and list_elems = expr list

  type type_ = type_atom list
  and type_atom = Tid of id | Type of type_

  type decl = ValDecl of id * expr | TypeDecl of id * (id * type_ option) list
  type prog = decl list
end

let keywords = Set.of_list (module String) [ "fun"; "let"; "in"; "vd"; "td" ]

let id : AST.id t =
  lexeme
    (trye
       (let%bind first = letter in
        let%bind rest = many (first_ok (first_ok letter digit) (char '_')) in
        let x = String.of_char_list (first :: rest) in
        if Set.mem keywords x then fail else return x))

let keyword (s : string) : unit t =
  let%bind _ = string s in
  let%map _ = spaces_1 in
  ()

(* TODO: this will result on a horrible disaster if there are too many digits *)
and int : AST.int t =
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
  AST.Binding (x, e1, e2)

and lambda () =
  let%bind _ = trye (keyword "fun") in
  let%bind x = id in
  let%bind _ = symbol "->" in
  let%map e = expr () in
  AST.Lambda (x, e)

(* TODO: make this chain_right_1 *)
and append () =
  chain_left_1 (add ())
    (let%map _ = symbol "++" in
     fun l r -> AST.BinOp (l, AST.Append, r))

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
       | "+" -> AST.BinOp (l, AST.Plus, r)
       | "-" -> AST.BinOp (l, AST.Sub, r)
       | _ -> assert false)

and mul () =
  chain_left_1 (apply ())
    (let%map x = first_ok (symbol "*") (symbol "/") in
     fun l r ->
       match x with
       | "*" -> AST.BinOp (l, AST.Mul, r)
       | "/" -> AST.BinOp (l, AST.Div, r)
       | _ -> assert false)

and apply () =
  let%bind x = atom () in
  let%map xs = many (atom ()) in
  List.fold ~init:x ~f:(fun f x -> AST.Apply (f, x)) xs

and atom () =
  choice
    [
      (let%map x = int in
       AST.Int x);
      (let%map x = id in
       AST.Id x);
      (let%bind _ = symbol "(" in
       let%bind e = expr () in
       let%map _ = symbol ")" in
       AST.Group e);
      (let%bind _ = symbol "[" in
       let%bind es = sep_by_1 ~sep:(symbol ",") (expr ()) in
       let%map _ = symbol "]" in
       AST.List es);
    ]

let val_decl =
  let%bind _ = keyword "vd" in
  let%bind x = id in
  let%bind _ = symbol ":=" in
  let%map e = expr () in
  AST.ValDecl (x, e)

let rec type_ () : AST.type_ t =
  let%bind first = type_atom () in
  let%map rest =
    many
      (let%bind _ = symbol "*" in
       let%map ta = type_atom () in
       ta)
  in
  first :: rest

and type_atom () : AST.type_atom t =
  choice
    [
      (let%map x = id in
       AST.Tid x);
      (let%bind _ = symbol "(" in
       let%bind t = type_ () in
       let%map _ = symbol ")" in
       AST.Type t);
    ]

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
           (let%map t = type_ () in
            Some t)
       in
       (y, t))
  in
  AST.TypeDecl (x, ts)

let prog =
  fully
    (let%bind vd = many (first_ok val_decl type_decl) in
     let%map _ = spaces in
     vd)
