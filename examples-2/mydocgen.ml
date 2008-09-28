class my_gen =
  object(self)
    inherit Odoc_html.html

    (** Return HTML code for the given text of a bar tag. *)
    method html_of_img t = 
      let s = Odoc_info.string_of_text t in
      "<img src=\"" ^ s ^ "\"/>"

    initializer
      tag_functions <- ("img", self#html_of_img) :: tag_functions
  end

let my_generator = new my_gen
let _ = Odoc_args.set_doc_generator 
          (Some (my_generator :> Odoc_args.doc_generator))


