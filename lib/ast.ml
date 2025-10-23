open! Base

type nonrec int = int
type id = string [@@deriving eq]
type ty_id = string [@@deriving eq]
type ty_var = string [@@deriving eq]

type bin_op =
  | Plus
  | Sub
  | Mul
  | Div
  | Append
[@@deriving eq]

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
[@@deriving eq]

type ty =
  | TyId of id
  | TyApp of ty * ty
  | TyProd of ty * ty
  | TyFun of ty * ty
[@@deriving eq]

type decl =
  | ValDecl of id * expr
  | TypeDecl of id * ty_var list * (id * ty option) list
[@@deriving eq]

type prog = decl list [@@deriving eq]
