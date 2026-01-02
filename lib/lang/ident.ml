open! Base

module type S = sig
  type t

  val create : Monotonic.t -> String.t -> t
  val to_string : t -> string
  val sexp_of_t : t -> Sexp.t

  include Comparable.S with type t := t
end

module Make (Item : sig
    val name : string
  end) : S = struct
  module T = struct
    type t =
      { index : Monotonic.t
      ; str : string
      }
    [@@deriving sexp_of]

    let create index str = { index; str }

    let to_string ident =
      Item.name ^ "[" ^ Monotonic.to_string ident.index ^ "]{" ^ ident.str ^ "}"
    ;;

    let compare x y = Monotonic.compare x.index y.index
  end

  include T
  include Comparable.Make (T)
end
