type nonrec int = int
type id = string
type ty_id = string
type ty_var = string
type bin_op = Plus | Sub | Mul | Div | Append

type expr =
  | Int of int
  | Id of id
  | Apply of expr * expr
  | Group of expr
  | Lambda of id * expr
  | Binding of id * expr * expr
  | List of expr list
  | Tuple of expr list
  | BinOp of expr * bin_op * expr

type ty = TyId of id | TyProd of ty * ty | TyFun of ty * ty
type decl = ValDecl of id * expr | TypeDecl of id * (id * ty option) list
type prog = decl list
