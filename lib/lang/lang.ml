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

module Raw = Raw
module Core = Core
module Term_env : Term_env_pub = Term_env
module Constr_env : Constr_env_pub = Constr_env
module Renamer = Renamer
module Type_env : Type_env_pub = Type_env
module Analyser_ctx = Analyser_ctx
module Scheme = Scheme
module Type = Type
module Var_ident = Var_ident
module Type_ident = Type_ident
module Constr_ident = Constr_ident
module Analyser_state = Analyser_state
