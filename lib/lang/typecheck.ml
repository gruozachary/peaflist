open! Base
open Result
open Let_syntax

type gen_var = GenVar of int
type uni_var = UniVar of int

type ty =
  | TUni of tu ref
  | TGen of gen_var
  | TFun of ty * ty
  | TProd of ty list
  | TCon of Type_ident.t * ty list

and tu =
  | Unbound of uni_var
  | Link of ty

type scheme = Forall of gen_var list * ty

let rec prune = function
  | TUni ({contents = Link ty } as tu_ref) ->
    let root = prune ty in
    tu_ref := Link root;
    root
  | ty -> ty

let rec unify ty1 ty2 =
  let unify_lists tys tys' =
    let%bind ty_pairs =
      match List.zip tys tys' with
      | List.Or_unequal_lengths.Ok ty_pairs -> return ty_pairs
      | List.Or_unequal_lengths.Unequal_lengths -> fail "Type unification failed"
    in
    let%map _ = List.map ~f:(fun (ty, ty') -> unify ty ty') ty_pairs |> Result.all in
    ()
  in
  match prune ty1, prune ty2 with
  | TUni tu_ref, TUni tu_ref' when phys_equal tu_ref tu_ref' -> return ()
  | TUni tu_ref, ty | ty, TUni tu_ref -> return (tu_ref := Link ty)
  | TFun (ty_fun, ty_arg), TFun (ty_fun', ty_arg') ->
    let%bind _ = unify ty_fun ty_fun' in
    unify ty_arg ty_arg'
  | TProd tys, TProd tys' -> unify_lists tys tys'
  | TCon (ident, tys), TCon (ident', tys') when Type_ident.equal ident ident' ->
    unify_lists tys tys'
  | TGen (GenVar x), TGen (GenVar y) when equal_int x y -> return ()
  | _ -> fail "Type unification failed"
;;
