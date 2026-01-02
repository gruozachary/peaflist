open! Base

type t =
  { env : Term_env.t
  ; tenv : Type_env.t
  ; cenv : Constr_env.t
  ; state : Analyser_state.t
  ; ident_renamer : Var_ident.t Renamer.t
  ; type_ident_renamer : Type_ident.t Renamer.t
  ; constr_ident_renamer : Constr_ident.t Renamer.t
  }

let fetch_and_lookup ctx ~ident_str =
  let open Option.Let_syntax in
  let%bind ident = Renamer.fetch ctx.ident_renamer ~str:ident_str in
  let%map scheme = Term_env.lookup ctx.env ~id:ident in
  ident, scheme
;;

let declare_and_introduce ctx ~ident_str ~scheme =
  let ident, r =
    Renamer.declare_and_fetch
      ctx.ident_renamer
      ~heart:(Analyser_state.ident_renamer_heart ctx.state)
      ~str:ident_str
  in
  ( ident
  , { ctx with ident_renamer = r; env = Term_env.introduce ctx.env ~id:ident ~sc:scheme }
  )
;;

let type_fetch_and_lookup ctx ~ident_str =
  let open Option.Let_syntax in
  let%bind ident = Renamer.fetch ctx.type_ident_renamer ~str:ident_str in
  let%map arity = Type_env.lookup ctx.tenv ~id:ident in
  ident, arity
;;

let type_declare_and_introduce ctx ~ident_str ~entry =
  let open Option.Let_syntax in
  let%map () =
    match Renamer.fetch ctx.type_ident_renamer ~str:ident_str with
    | Option.Some _ -> Option.None
    | Option.None -> Option.Some ()
  in
  let ident, r =
    Renamer.declare_and_fetch
      ctx.type_ident_renamer
      ~heart:(Analyser_state.type_ident_renamer_heart ctx.state)
      ~str:ident_str
  in
  let tenv = Type_env.introduce ctx.tenv ~id:ident ~data:entry in
  ident, { ctx with type_ident_renamer = r; tenv }
;;

(* TODO: Consider an abstraction*)
let constr_fetch_and_lookup ctx ~ident_str =
  let open Option.Let_syntax in
  let%bind ident = Renamer.fetch ctx.constr_ident_renamer ~str:ident_str in
  let%map entry = Constr_env.lookup ctx.cenv ~ident in
  ident, entry
;;

let constr_declare_and_introduce ctx ~ident_str ~entry =
  let ident, r =
    Renamer.declare_and_fetch
      ctx.constr_ident_renamer
      ~heart:(Analyser_state.constr_ident_renamer_heart ctx.state)
      ~str:ident_str
  in
  ( ident
  , { ctx with
      constr_ident_renamer = r
    ; cenv = Constr_env.introduce ctx.cenv ~ident ~data:entry
    } )
;;

module Env = struct
  let get ctx = ctx.env
  let map ctx ~f = { ctx with env = f ctx.env }
end

module Tenv = struct
  let get ctx = ctx.tenv
  let map ctx ~f = { ctx with tenv = f ctx.tenv }
end

module C_env = struct
  let get ctx = ctx.cenv
  let map ctx ~f = { ctx with cenv = f ctx.cenv }
end

module State = struct
  let get ctx = ctx.state
end

module Var_ident_renamer = struct
  let get ctx = ctx.ident_renamer
  let map ctx ~f = { ctx with ident_renamer = f ctx.ident_renamer }
end

module Type_ident_renamer = struct
  let get ctx = ctx.type_ident_renamer
  let map ctx ~f = { ctx with type_ident_renamer = f ctx.type_ident_renamer }
end

module Constr_ident_renamer = struct
  let get ctx = ctx.constr_ident_renamer
  let map ctx ~f = { ctx with constr_ident_renamer = f ctx.constr_ident_renamer }
end

let empty () =
  let state = Analyser_state.create () in
  let ctx =
    { env = Term_env.empty ()
    ; state
    ; tenv = Type_env.empty
    ; cenv = Constr_env.empty
    ; ident_renamer = Renamer.empty (Analyser_state.ident_renamer_heart state)
    ; type_ident_renamer = Renamer.empty (Analyser_state.type_ident_renamer_heart state)
    ; constr_ident_renamer =
        Renamer.empty (Analyser_state.constr_ident_renamer_heart state)
    }
  in
  let int_ident, ctx =
    type_declare_and_introduce ctx ~ident_str:"int" ~entry:{ arity = 0; constrs = [] }
    |> Option.value_exn
  in
  let int_tcon = Type.TCon (int_ident, []) in
  let int_bin_op_scheme =
    Type.TFun (int_tcon, Type.TFun (int_tcon, int_tcon)) |> Scheme.of_type
  in
  let _, ctx = declare_and_introduce ctx ~ident_str:"+" ~scheme:int_bin_op_scheme in
  let _, ctx = declare_and_introduce ctx ~ident_str:"-" ~scheme:int_bin_op_scheme in
  let _, ctx = declare_and_introduce ctx ~ident_str:"*" ~scheme:int_bin_op_scheme in
  let _, ctx = declare_and_introduce ctx ~ident_str:"/" ~scheme:int_bin_op_scheme in
  ctx
;;
