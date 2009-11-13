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

let supported = false

let float_of_num n = failwith "Cairost.float_of_num : not supported"

let emit_pdf ?msg_error s c = failwith "Cairost.emit_pdf: not supported"
let emit_png s c = failwith "Cairost.emit_png: not supported"
let emit_ps s c = failwith "Cairost.emit_ps: not supported"
let emit_svg s c = failwith "Cairost.emit_svg: not supported"
let emit_pdfs s c = failwith "Cairost.emit_pdfs: not supported"

let dump_pdf _ = failwith "Cairost.dump_pdf : not supported"
let dump_pdfs _ = failwith "Cairost.dump_pdfs : not supported"
let dump_ps _ = failwith "Cairost.dump_ps : not supported"

let set_prelude _ = failwith "Cairost.set_prelude : not supported"
let set_t1disasm _ = failwith "Cairost.set_t1disasm : not supported"

let generate_pdfs _ _ = failwith "Cairost.generate_pdfs : not supported"

type cairo_t = unit

let emit_cairo  _ _ _ = failwith "Cairost.emit_cairo : not supported"
