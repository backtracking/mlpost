
let supported = false

let float_of_num n = failwith "Cairost.float_of_num : not supported"

let emit_pdf ?msg_error s c = failwith "Cairost.emit_pdf: not supported"
let emit_png s c = failwith "Cairost.emit_png: not supported"
let emit_ps s c = failwith "Cairost.emit_ps: not supported"
let emit_svg s c = failwith "Cairost.emit_svg: not supported"
let emit_pdfs s c = failwith "Cairost.emit_pdfs: not supported"

let dump_pdf _ = failwith "Cairost.dump_pdf : not supported"
let dump_pdfs _ = failwith "Cairost.dump_pdfs : not supported"

let set_prelude _ = failwith "Cairost.set_prelude : not supported"
let set_t1disasm _ = failwith "Cairost.set_t1disasm : not supported"

let generate_pdfs _ _ = failwith "Cairost.generate_pdfs : not supported"

type cairo_t = unit

let emit_cairo  _ _ = failwith "Cairost.emit_cairo : not supported"
