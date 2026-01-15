open! Base

module Make_var () : sig
  type t

  val zero : t
  val succ : t -> t
  val sexp_of_t : t -> Sexp.t

  include Comparable.S with type t := t
end = struct
  module T = struct
    type t = int [@@deriving compare, sexp_of]

    let zero = 0
    let succ x = x + 1
  end

  include T
  include Comparable.Make (T)
end

module Gen_var = Make_var ()
module Uni_var = Make_var ()

module Type = struct
  type phantom_ununified
  type phantom_unified

  type _ t =
    | Gen : Gen_var.t -> 'any t
    | Uni : uni ref -> phantom_ununified t
    | Fun : 'any t * 'any t -> 'any t
    | Prod : 'any t list -> 'any t
    | Con : Type_ident.t * 'any t list -> 'any t

  and any_t = Any : 'any t -> any_t

  and uni =
    | Unbound of Uni_var.t
    | Link of phantom_ununified t

  let rec sexp_of_t : type a. a t -> Sexp.t = function
    | Gen gen_var -> Sexp.List [ Sexp.Atom "Gen"; Gen_var.sexp_of_t gen_var ]
    | Uni { contents = Unbound uni_var } ->
      Sexp.List [ Sexp.Atom "Unbound"; Uni_var.sexp_of_t uni_var ]
    | Uni { contents = Link ty } -> Sexp.List [ Sexp.Atom "Link"; sexp_of_t ty ]
    | Fun (ty_arg, ty_res) ->
      Sexp.List [ Sexp.Atom "Fun"; sexp_of_t ty_arg; sexp_of_t ty_res ]
    | Prod tys -> Sexp.List [ Sexp.Atom "Prod"; Sexp.List (List.map ~f:sexp_of_t tys) ]
    | Con (ident, tys) ->
      Sexp.List
        [ Sexp.Atom "Con"
        ; Type_ident.sexp_of_t ident
        ; Sexp.List (List.map ~f:sexp_of_t tys)
        ]
  ;;

  type unified_t = phantom_unified t
  type ununified_t = phantom_ununified t
end

module Scheme = struct
  type _ t = Forall : Gen_var.t list * 'any Type.t -> 'any t
  type any_t = Any : 'any t -> any_t
  type unified_t = Type.phantom_unified t
  type ununified_t = Type.phantom_ununified t

  let sexp_of_t : type a. a t -> Sexp.t =
    fun (Forall (gen_vars, ty)) ->
    Sexp.List
      [ Sexp.Atom "Forall"
      ; Sexp.List (List.map ~f:Gen_var.sexp_of_t gen_vars)
      ; Type.sexp_of_t ty
      ]
  ;;
end
