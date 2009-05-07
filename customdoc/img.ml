class my_gen =
  object(self)
    inherit Odoc_html.html

    (** Return HTML code for the given text of a bar tag. *)
    method html_of_img t = 
      match t with 
      | [] -> ""
      | (x::r) ->
          begin
          match x with
          | Odoc_info.Raw s -> Format.sprintf "<img alt=\"%s\" src=\"%s\"/>" s s
          | _ -> ""
          end
          
    initializer
      tag_functions <- ("img", self#html_of_img) :: tag_functions
  end

let my_generator = new my_gen
let _ = 
  Odoc_args.set_doc_generator 
         (Some my_generator :> Odoc_args.doc_generator option) ;
  (* we need to deactivate the -html option of ocamldoc, otherwise our generator
   * is overwritten by the standard html generator. Ocamlbuild gives the -html
   * option to ocamldoc, so this is really required *)
  Odoc_args.add_option ("-html", Arg.Unit (fun () -> ()), "")

