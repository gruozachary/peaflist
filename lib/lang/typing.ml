open! Base

module Make_var () : sig
  type t

  val zero : t
  val succ : t -> t
  val sexp_of_t : t -> Sexp.t
  val to_string : t -> string

  include Comparable.S with type t := t
end = struct
  module T = struct
    type t = int [@@deriving compare, sexp_of]

    let zero = 0
    let succ x = x + 1
    let to_string = Int.to_string
  end

  include T
  include Comparable.Make (T)
end

module Gen_var = Make_var ()
module Uni_var = Make_var ()

let label_gen_vars
  :  (Gen_var.t, Gen_var.comparator_witness) Set.t
  -> (Gen_var.t, string, Gen_var.comparator_witness) Map.t
  =
  fun gen_vars ->
  let _, mapping =
    Set.fold
      gen_vars
      ~init:(97, Map.empty (module Gen_var))
      ~f:(fun (i, mapping) gen_var ->
        ( i + 1
        , Option.value_map (Char.of_int i) ~default:mapping ~f:(fun c ->
            Map.set mapping ~key:gen_var ~data:(String.of_char c)) ))
  in
  mapping
;;

let gen_var_to_string_pretty mapping gen_var =
  "'" ^ (Map.find mapping gen_var |> Option.value ~default:(Gen_var.to_string gen_var))
;;

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

  type unified_t = phantom_unified t
  type ununified_t = phantom_ununified t

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

  let prec : type a. a t -> int = function
    | Uni _ -> 3
    | Gen _ -> 3
    | Fun _ -> 1
    | Prod _ -> 2
    | Con _ -> 3
  ;;

  let rec gen_vars_of_ty : type a. a t -> (Gen_var.t, Gen_var.comparator_witness) Set.t =
    function
    | Gen gen_var -> Set.singleton (module Gen_var) gen_var
    | Uni _ -> Set.empty (module Gen_var)
    | Fun (ty_arg, ty_res) -> Set.union (gen_vars_of_ty ty_arg) (gen_vars_of_ty ty_res)
    | Prod tys -> List.map ~f:gen_vars_of_ty tys |> Set.union_list (module Gen_var)
    | Con (_, tys) -> List.map ~f:gen_vars_of_ty tys |> Set.union_list (module Gen_var)
  ;;

  let rec to_string
    : type a.
      ?mapping:(Gen_var.t, string, Gen_var.comparator_witness) Map.t -> a t -> string
    =
    fun ?mapping ty ->
    let mapping = Option.value mapping ~default:(gen_vars_of_ty ty |> label_gen_vars) in
    let go ty' =
      if prec ty' < prec ty
      then "(" ^ to_string ~mapping ty' ^ ")"
      else to_string ~mapping ty'
    in
    match ty with
    | Uni { contents = Unbound uni_var } -> "unbound" ^ Uni_var.to_string uni_var
    | Uni { contents = Link ty } -> "link" ^ go ty
    | Gen gen_var -> gen_var_to_string_pretty mapping gen_var
    | Fun (t, t') -> go t ^ " -> " ^ go t'
    | Prod ts ->
      List.map ~f:go ts
      |> List.intersperse ~sep:" * "
      |> List.fold ~init:"" ~f:String.append
    | Con (ident, []) -> Type_ident.to_string ident
    | Con (ident, [ t ]) -> go t ^ " " ^ Type_ident.to_string ident
    | Con (ident, ts) ->
      "("
      ^ (List.map ~f:go ts
         |> List.intersperse ~sep:" "
         |> List.fold ~init:"" ~f:String.append)
      ^ ") "
      ^ Type_ident.to_string ident
  ;;
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

  let to_string
    : type a.
      ?mapping:(Gen_var.t, string, Gen_var.comparator_witness) Map.t -> a t -> string
    =
    fun ?mapping (Forall (qs, ty)) ->
    let mapping =
      Option.value mapping ~default:(Type.gen_vars_of_ty ty |> label_gen_vars)
    in
    (List.map qs ~f:(fun gen_var -> gen_var_to_string_pretty mapping gen_var)
     |> List.intersperse ~sep:" "
     |> List.fold ~init:"forall " ~f:String.append)
    ^ " . "
    ^ Type.to_string ~mapping ty
  ;;
end
