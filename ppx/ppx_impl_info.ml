open Ppxlib

let name = "get_impl_info"

let expand ~ctxt name =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let open Ast_builder.Default in
  let label_expr = estring ~loc name in
  let module_expr =
    pexp_pack ~loc
      {
        pmod_desc = Pmod_ident { txt = lident name; loc };
        pmod_loc = loc;
        pmod_attributes = [];
      }
  in

  pexp_tuple ~loc [ label_expr; module_expr ]

let my_extension =
  Extension.V3.declare name Extension.Context.expression
    Ast_pattern.(single_expr_payload (estring __))
    expand

let rule = Ppxlib.Context_free.Rule.extension my_extension
let () = Driver.register_transformation ~rules:[ rule ] name
