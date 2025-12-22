open! Base

type t =
  { env : Term_env.t
  ; tenv : Type_env.t
  ; state : Analyser_state.t
  }

let empty () =
  { env = Term_env.empty (); state = Analyser_state.create (); tenv = Type_env.empty }
;;

module Env = struct
  let get ctx = ctx.env
  let map ctx ~f = { ctx with env = f ctx.env }
end

module Tenv = struct
  let get ctx = ctx.tenv
  let map ctx ~f = { ctx with tenv = f ctx.tenv }
end

module State = struct
  let get ctx = ctx.state
end
