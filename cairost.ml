(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Johannes Kanig, Stephane Lescuyer                       *)
(*  Jean-Christophe Filliatre, Romain Bardou and Francois Bobot           *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

IFDEF CAIRO THEN
let supported = true

open Icairost

let set_verbosity = set_verbosity
let () = Types.add_set_verbosity set_verbosity

let emit_pdf = emit_pdf
let emit_ps = emit_ps
let emit_png = emit_png
let emit_svg = emit_svg
let emit_pdfs = emit_pdfs

type cairo_t = Cairo.t

let emit_cairo = emit_cairo
(*let emit_cairo = fun x -> ()*)

let dump_pdf () = 
  Queue.iter (fun (_,fname,fig) -> 
		let pdfname = (fname^".pdf") in
		  try
		    emit_pdf  pdfname fig
		  with
		    | Cairo.Error status -> 
			Format.printf "An@ internal@ error@ occured@ during@\
 the generation@ of@ %s@ with@ Cairo :@ %s@."
			  pdfname (Cairo.string_of_status status)
		    | error -> Format.printf "An@ internal@ error@ occured@\
 during@ the@ generation@ of@ %s :@ %s@."
			pdfname (Printexc.to_string error)
	     ) Metapost.figures

let dump_pdfs fname = 
  let figs = 
    List.rev (Queue.fold (fun l (_,_,x) ->  x::l) [] Metapost.figures) in
  emit_pdfs (fname^".pdf") figs

let generate_pdfs pdffile figs = List.iter 
  (fun (i,fig) -> emit_pdf ~msg_error:100. 
     (Printf.sprintf "%s-%i.pdf" pdffile i) fig) figs

let dump_ps () = 
  Queue.iter (fun (_,fname,fig) -> emit_ps (fname^".ps") fig) Metapost.figures

let dump_png () = 
  Queue.iter (fun (_,fname,fig) -> emit_png (fname^".png") fig)
    Metapost.figures

let dump_svg () = 
  Queue.iter (fun (_,fname,fig) -> emit_svg (fname^".svg") fig)
    Metapost.figures

ELSE

let supported = false

let set_verbosity _ = failwith "Cairost.set_verbosity : not supported"

let float_of_num n = failwith "Cairost.float_of_num : not supported"

let emit_pdf ?msg_error s c = failwith "Cairost.emit_pdf: not supported"
let emit_png s c = failwith "Cairost.emit_png: not supported"
let emit_ps s c = failwith "Cairost.emit_ps: not supported"
let emit_svg s c = failwith "Cairost.emit_svg: not supported"
let emit_pdfs s c = failwith "Cairost.emit_pdfs: not supported"

let dump_pdf _ = failwith "Cairost.dump_pdf : not supported"
let dump_pdfs _ = failwith "Cairost.dump_pdfs : not supported"
let dump_ps _ = failwith "Cairost.dump_ps : not supported"
let dump_png _ = failwith "Cairost.dump_png : not supported"

let set_prelude _ = failwith "Cairost.set_prelude : not supported"
let set_t1disasm _ = failwith "Cairost.set_t1disasm : not supported"

let generate_pdfs _ _ = failwith "Cairost.generate_pdfs : not supported"

type cairo_t = unit

let emit_cairo  _ _ _ = failwith "Cairost.emit_cairo : not supported"

END
