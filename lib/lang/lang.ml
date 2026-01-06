module type Term_env_pub = sig
    type t

    val lookup : t -> id:Var_ident.t -> Scheme.t option
  end
  with type t = Term_env.t

module type Type_env_pub = sig
    type t

    type entry =
      { arity : int
      ; constrs : Constr_ident.t List.t
      }

    val lookup : t -> id:Type_ident.t -> entry Option.t
  end
  with type t = Type_env.t

module type Constr_env_pub = sig
  type t

  type entry =
    { parent : Type_ident.t
    ; tag : int
    ; arg_scheme_opt : Scheme.t Option.t
    ; res_scheme : Scheme.t
    }

  val lookup : t -> ident:Constr_ident.t -> entry Option.t
end

module type Analyser_ctx_pub = sig
    type t

    val empty : unit -> t
    val fetch_and_lookup : t -> ident_str:string -> (Var_ident.t * Scheme.t) option

    val type_fetch_and_lookup
      :  t
      -> ident_str:string
      -> (Type_ident.t * Type_env.entry) option

    module Env : sig
      val get : t -> Term_env.t
    end

    module Tenv : sig
      val get : t -> Type_env.t
    end

    module C_env : sig
      val get : t -> Constr_env.t
    end

    module Var_ident_renamer : sig
      val get : t -> Var_ident.t Renamer.t
    end
  end
  with type t = Analyser_ctx.t

module Raw = Raw
module Core = Core
module Term_env : Term_env_pub = Term_env
module Constr_env : Constr_env_pub = Constr_env
module Renamer = Renamer
module Type_env : Type_env_pub = Type_env
module Analyser_ctx : Analyser_ctx_pub = Analyser_ctx
module Scheme = Scheme
module Type = Type
module Var_ident = Var_ident
module Type_ident = Type_ident
module Constr_ident = Constr_ident
