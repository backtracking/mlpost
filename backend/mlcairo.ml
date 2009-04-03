open Point
open Format
let info = ref false

let create create_surface draw height width out_file =
  if !info then printf "height = %f, width = %f@." height width;
  let oc = open_out out_file in
  let s = create_surface oc ~width_in_points:width ~height_in_points:height in
  let cr = Cairo.create s in
  draw cr;
  if !info then printf "Clean up surface_finish ...@.";
  Cairo.surface_finish s;
  if !info then printf "Clean up close file ...@.";
  close_out oc

let create_ps = create Cairo_ps.surface_create_for_channel

let create_pdf = create Cairo_pdf.surface_create_for_channel


let emit_gen create fname fig = 
  let ({x=xmin;y=ymin},{x=xmax;y=ymax}) = Picture_lib.bounding_box fig in
  let height = ymax -. ymin in
  let width = xmax -. xmin in
  let fig = Picture_lib.shift fig (-.xmin) (-.ymin) in
  create (fun cr -> Picture_lib.Cairo.draw cr fig) height width (fname ^ ".pdf")

let emit_pdf = emit_gen create_pdf
let emit_ps = emit_gen create_ps
