open! Base

type t =
  { env : Term_env.t
  ; tenv : Type_env.t
  ; state : Analyser_state.t
  ; renamer : Renamer.t
  }

let empty () =
  let state = Analyser_state.create () in
  { env = Term_env.empty ()
  ; state
  ; tenv = Type_env.empty
  ; renamer = Renamer.empty (Analyser_state.renamer_heart state)
  }
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

module Renamer = struct
  let get ctx = ctx.renamer
  let map ctx ~f = { ctx with renamer = f ctx.renamer }
end
