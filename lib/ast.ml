open! Base
open Sexplib.Std

type nonrec int = int
type id = string [@@deriving sexp]
type ty_id = string [@@deriving sexp]
type ty_var = string [@@deriving sexp]
type bin_op = Plus | Sub | Mul | Div | Append [@@deriving sexp]

type expr =
  | Int of int
  | Id of id
  | Apply of expr * expr
  | Group of expr
  | Lambda of id * expr
  | Binding of id * expr * expr
  | Match of expr * (expr * expr) list
  | List of expr list
  | Tuple of expr list
  | BinOp of expr * bin_op * expr
[@@deriving sexp]

type ty = TyId of id | TyApp of ty * ty | TyProd of ty * ty | TyFun of ty * ty
[@@deriving sexp]

type decl =
  | ValDecl of id * expr
  | TypeDecl of id * ty_var list * (id * ty option) list
[@@deriving sexp]

type prog = decl list [@@deriving sexp]
