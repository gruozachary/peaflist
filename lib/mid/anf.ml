open! Base
open Lang

module Ctx = struct
  type t =
    { renamer : Var_ident.t Renamer.t
    ; renamer_heart : Var_ident.t Renamer.heart
    }
end

module Expr = struct
  type atomic
  type compound

  type _ t =
    | Int : Int.t -> atomic t
    | Var : Var_ident.t * Type.t -> atomic t
    | Lambda : Var_ident.t * Type.t * 'any t -> atomic t
    | Constr : Constr_ident.t * atomic t List.t * Type.t -> compound t
    | Apply : atomic t * atomic t * Type.t -> compound t
    | Let : Var_ident.t * Scheme.t * 'any t * 'any t -> compound t
    | Tuple : atomic t List.t -> compound t
  (* match will be done later *)
end
