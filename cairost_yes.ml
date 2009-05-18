let supported = true

open Icairost

let float_of_num = LookForTeX.num

let emit_pdf = emit_pdf
let emit_ps = emit_ps
let emit_png = emit_png
let emit_svg = emit_svg
let emit_pdfs = emit_pdfs

let emit_cairo : (float -> float -> Cairo.t) -> Command.t -> unit = emit_cairo

let dump_pdf () = Queue.iter (fun (_,fname,fig) -> emit_pdf (fname^".pdf") fig) Metapost.figures
let dump_pdfs fname = 
  let figs = List.rev (Queue.fold (fun l (_,_,x) ->  x::l) [] Metapost.figures) in
  emit_pdfs (fname^".pdf") figs

let generate_pdfs pdffile figs = List.iter 
  (fun (i,fig) -> emit_pdf ~msg_error:100. 
     (Printf.sprintf "%s-%i.pdf" pdffile i) fig) figs
