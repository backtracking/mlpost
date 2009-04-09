let supported = true

(*open Backend.Cairost

let emit_pdf = emit_pdf
let emit_ps = emit_ps
let emit_png = emit_png
let emit_gtk = emit_gtk
let emit_svg = emit_svg
let emit_pdfs = emit_pdfs
*)

let emit_pdf s c = failwith "Cairo.emit_pdf: not supported"
let emit_png s c = failwith "Cairo.emit_png: not supported"
let emit_ps s c = failwith "Cairo.emit_ps: not supported"
let emit_svg s c = failwith "Cairo.emit_svg: not supported"
let emit_pdfs s c = failwith "Cairo.emit_svg: not supported"
