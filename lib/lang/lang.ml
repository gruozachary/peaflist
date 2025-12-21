module type Gamma_pub = sig
    type t

    val lookup : t -> id:string -> Scheme.t option
  end
  with type t = Gamma.t

module type Ctx_pub = sig
    type t

    val empty : unit -> t

    module Env : sig
      val get : t -> Gamma.t
    end

    module Tenv : sig
      val get : t -> Ty_env.t
    end
  end
  with type t = Ctx.t

module type Scheme_pub = sig
    type t

    val to_string : t -> string
  end
  with type t = Scheme.t

module type Tau_pub = sig
    type t

    val to_string : t -> string
  end
  with type t = Tau.t

module type Ty_env_pub = sig
    type t
    type arity = int

    val lookup : t -> id:string -> arity Option.t
  end
  with type t = Ty_env.t

module Ast = Ast
module Gamma : Gamma_pub = Gamma
module Ctx : Ctx_pub = Ctx
module Scheme : Scheme_pub = Scheme
module Tau : Tau_pub = Tau
module Ty_env : Ty_env_pub = Ty_env
