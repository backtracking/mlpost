
let supported = false

let float_of_num n = failwith "Cairost.float_of_num : not supported"

let emit_pdf s c = failwith "Cairost.emit_pdf: not supported"
let emit_png s c = failwith "Cairost.emit_png: not supported"
let emit_ps s c = failwith "Cairost.emit_ps: not supported"
let emit_svg s c = failwith "Cairost.emit_svg: not supported"
let emit_pdfs s c = failwith "Cairost.emit_svg: not supported"

module C : Cairost_sig.S = struct end
