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

let supported = true

open Icairost

let emit_pdf = emit_pdf
let emit_ps = emit_ps
let emit_png = emit_png
let emit_svg = emit_svg
let emit_pdfs = emit_pdfs

type cairo_t = Cairo.t

let emit_cairo = emit_cairo
(*let emit_cairo = fun x -> ()*)

let dump_pdf () = 
  Queue.iter (fun (_,fname,fig) -> emit_pdf (fname^".pdf") fig) Metapost.figures

let dump_pdfs fname = 
  let figs = List.rev (Queue.fold (fun l (_,_,x) ->  x::l) [] Metapost.figures) in
  emit_pdfs (fname^".pdf") figs

let generate_pdfs pdffile figs = List.iter 
  (fun (i,fig) -> emit_pdf ~msg_error:100. 
     (Printf.sprintf "%s-%i.pdf" pdffile i) fig) figs

let dump_ps () = 
  Queue.iter (fun (_,fname,fig) -> emit_ps (fname^".ps") fig) Metapost.figures

let dump_png () = 
  Queue.iter (fun (_,fname,fig) -> emit_png (fname^".png") fig) Metapost.figures
