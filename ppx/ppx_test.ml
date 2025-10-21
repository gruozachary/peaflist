open Ppxlib

let expand_sqc_test ~loc ~path:_ (expr : expression) =
  [%stri
    let%test _ =
      let ast_result =
        Sqc.Peasec.exec Sqc.Parser.prog
          [%e
            Ast_builder.Default.pexp_field ~loc expr
              (Ast_builder.Default.Located.lident ~loc "str")]
      in
      match
        Option.map ast_result
          ~f:
            (Sqc.Ast.equal_prog
               [%e
                 Ast_builder.Default.pexp_field ~loc expr
                   (Ast_builder.Default.Located.lident ~loc "ast")])
      with
      | Some x -> x
      | _ -> false]

let ext =
  Extension.declare "sqc_tests" Extension.Context.structure_item
    Ast_pattern.(single_expr_payload __)
    expand_sqc_test

let () = Driver.register_transformation ~extensions:[ ext ] "ppx_test"
