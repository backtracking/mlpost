type t = Tfm.t

open Tfm

(* compute index of the character *)
let to_abs_idx t i = i - t.file_hdr.bc

(* get info struct of character *)
let get_info t i = t.body.char_info.(to_abs_idx t i)

let char_width t c = t.body.width.((get_info t c).width_index)
let char_height t c = t.body.height.((get_info t c).height_index)
let char_depth t c = t.body.depth.((get_info t c).depth_index)
let char_italic t c = t.body.italic.((get_info t c).italic_index)
