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

let expand_many ~ctxt names =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  Ast_builder.Default.elist ~loc (List.map (expand ~ctxt) names)

let my_extension =
  let open Ast_pattern in
  let pat_expr =
    pexp_apply (estring __) (many (no_label (estring __)))
    ||| map_result ~f:(fun expr_fun -> expr_fun []) (estring __)
  in

  Extension.V3.declare name Extension.Context.expression
    (single_expr_payload pat_expr) (fun ~ctxt hd tl ->
      expand_many ~ctxt (hd :: tl))

let rule = Ppxlib.Context_free.Rule.extension my_extension
let () = Driver.register_transformation ~rules:[ rule ] name
