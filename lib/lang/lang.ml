module type Term_env_pub = sig
    type t

    val lookup : t -> id:string -> Scheme.t option
  end
  with type t = Term_env.t

module type Type_env_pub = sig
    type t
    type arity = int

    val lookup : t -> id:string -> arity Option.t
  end
  with type t = Type_env.t

module type Analyser_ctx_pub = sig
    type t

    val empty : unit -> t

    module Env : sig
      val get : t -> Term_env.t
    end

    module Tenv : sig
      val get : t -> Type_env.t
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
module Type_env : Type_env_pub = Type_env
module Analyser_ctx : Analyser_ctx_pub = Analyser_ctx
module Scheme : Scheme_pub = Scheme
module Type : Type_pub = Type
