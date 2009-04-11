let supported = true

open Icairost

let emit_pdf = emit_pdf
let emit_ps = emit_ps
let emit_png = emit_png
let emit_svg = emit_svg
let emit_pdfs = emit_pdfs

let emit_cairo : (float -> float -> Cairo.t) -> Command.t -> unit = emit_cairo
