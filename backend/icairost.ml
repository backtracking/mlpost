open Point_lib
open Format
let info = ref false

let create create_surface out_file (draw:Cairo.t -> unit) height width =
  if !info then printf "height = %f, width = %f@." height width;
  let oc = open_out out_file in
  let s = create_surface oc ~width_in_points:width ~height_in_points:height in
  let cr = Cairo.create s in
  draw cr;
  if !info then printf "Clean up surface_finish ...@.";
  Cairo.surface_finish s;
  if !info then printf "Clean up close file ...@.";
  close_out oc

let emit_gen create fig = 
  (*Format.printf "Fig : %a" Print.command fig;*)
  let fig = Compute.command fig in
  let ({x=xmin;y=ymin},{x=xmax;y=ymax}) = Picture_lib.bounding_box fig in
  let height = ymax -. ymin in
  let width = xmax -. xmin in
  let fig = Picture_lib.shift fig (-.xmin) (-.ymin) in
  create (fun cr -> 
            Picture_lib.ToCairo.draw cr width height fig
         ) height width

let emit_pdf fname = emit_gen (create Cairo_pdf.surface_create_for_channel fname)
let emit_ps fname = emit_gen (create Cairo_ps.surface_create_for_channel fname)
let emit_svg fname = emit_gen (create Cairo_svg.surface_create_for_channel fname)
let emit_png fname = emit_gen 
  (fun draw height width ->
     let width = int_of_float (ceil width) in
     let height = int_of_float (ceil height) in
     let surf = Cairo.image_surface_create Cairo.FORMAT_ARGB32 ~width ~height in
     let cr = (Cairo.create surf) in
     draw cr;
     Cairo.surface_finish surf;
     Cairo_png.surface_write_to_file surf fname)

let emit_cairo gencr (fig:Command.t) = emit_gen (fun draw height width -> draw (gencr height width)) fig

let emit_pdfs s figs = failwith "not implemented"
