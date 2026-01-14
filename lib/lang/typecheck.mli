type ctx

val empty : unit -> ctx
val basic : Rename.t -> ctx

val typecheck_expr
  :  ctx
  -> Rename.t
  -> Desugared_ast.Expr.t
  -> (Core_ast.Expr.t, string) result

val typecheck_decl
  :  ctx
  -> Rename.t
  -> Desugared_ast.Decl.t
  -> (Core_ast.Decl.t * ctx, string) result

val typecheck_prog
  :  ctx
  -> Rename.t
  -> Desugared_ast.Prog.t
  -> (Core_ast.Prog.t * ctx, string) result
