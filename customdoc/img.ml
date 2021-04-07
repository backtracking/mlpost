open Ppxlib

module B = Ast_builder.Make (struct
  let loc = Location.none
end)

let img_subst =
  let img_regexp = Str.regexp "^ *@img \\([a-z.]*\\)" in
  Str.global_replace img_regexp "{%html:<img alt=\"\\1\" src=\"img/\\1\"/>%}"

let rewrite_payload p =
  Ast_pattern.(parse (pstr (pstr_eval (estring __) __ ^:: nil))) Location.none p
    (fun s _ -> img_subst s)
  |> fun s -> PStr B.[ pstr_eval (estring s) [] ]

let rewriter =
  object
    inherit Ast_traverse.map

    method! attribute a =
      if a.attr_name.txt = "ocaml.text" || a.attr_name.txt = "ocaml.doc" then
        { a with attr_payload = rewrite_payload a.attr_payload }
      else a
  end

let () =
  let intf = rewriter#signature in
  let impl = rewriter#structure in
  Driver.register_transformation ~intf ~impl "mlpost_doc"
