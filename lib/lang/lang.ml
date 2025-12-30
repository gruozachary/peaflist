module type Term_env_pub = sig
    type t

    val lookup : t -> id:Ident.t -> Scheme.t option
  end
  with type t = Term_env.t

module type Renamer_pub = sig
    type 'mono t

    val fetch : 'mono t -> str:string -> 'mono Option.t
  end
  with type 'mono t = 'mono Renamer.t

module type Type_env_pub = sig
    type t
    type arity = int

    val lookup : t -> id:string -> arity Option.t
  end
  with type t = Type_env.t

module type Analyser_ctx_pub = sig
    type t

    val empty : unit -> t
    val fetch_and_lookup : t -> ident_str:string -> (Ident.t * Scheme.t) option

    module Env : sig
      val get : t -> Term_env.t
    end

    module Tenv : sig
      val get : t -> Type_env.t
    end

    module Ident_renamer : sig
      val get : t -> Ident.t Renamer.t
    end
  end
  with type t = Analyser_ctx.t

module type Scheme_pub = sig
    type t

    val to_string : t -> string
  end
  with type t = Scheme.t

module type Type_pub = sig
    type t

    val to_string : t -> string
  end
  with type t = Type.t

module Raw = Raw
module Core = Core
module Term_env : Term_env_pub = Term_env
module Renamer : Renamer_pub = Renamer
module Type_env : Type_env_pub = Type_env
module Analyser_ctx : Analyser_ctx_pub = Analyser_ctx
module Scheme : Scheme_pub = Scheme
module Type : Type_pub = Type
