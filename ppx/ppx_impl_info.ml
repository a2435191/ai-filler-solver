open Ppxlib

let name = "get_env"

let expand ~ctxt (name : label) =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  Ast_builder.Default.(
    pexp_tuple ~loc
      [
        pexp_constant ~loc (Pconst_char 'a');
        pexp_constant ~loc (Pconst_char 'b');
      ])

let my_extension =
  Extension.V3.declare name Extension.Context.expression
    Ast_pattern.(single_expr_payload (estring __))
    expand

let rule = Ppxlib.Context_free.Rule.extension my_extension
let () = Driver.register_transformation ~rules:[ rule ] name
