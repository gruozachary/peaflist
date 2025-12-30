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

let fetch_and_lookup ctx ~ident_str =
  let open Option.Let_syntax in
  let%bind ident = Renamer.fetch ctx.renamer ~str:ident_str in
  let%map scheme = ctx.env |> Term_env.lookup ~id:ident in
  ident, scheme
;;

let declare_and_introduce ctx ~ident_str ~scheme =
  let ident, r =
    Renamer.declare_and_fetch
      ctx.renamer
      ~heart:(Analyser_state.renamer_heart ctx.state)
      ~str:ident_str
  in
  ident, { ctx with renamer = r; env = Term_env.introduce ctx.env ~id:ident ~sc:scheme }
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
