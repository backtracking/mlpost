open Point_lib
open Picture_lib
module S = Spline_lib
open Format

let string = pp_print_string

module PS = struct
  let moveto fmt p = fprintf fmt " %f %f moveto " p.x p.y
  let curveto fmt p1 p2 p3 =
    fprintf fmt " %f %f %f %f %f %f curveto " p1.x p1.y p2.x p2.y p3.x p3.y
  let close_path fmt = string fmt " close_path "
  let newpath fmt = string fmt " newpath "
end

let curveto fmt s =
  let _, sb, sc, sd = Spline.explode s in
  PS.curveto fmt sb sc sd

let path fmt = function
  | S.Path p ->
      begin match p.S.pl with
      | [] -> assert false
      | (x::_) as l ->
          PS.newpath fmt;
          PS.moveto fmt (Spline.left_point x);
          Misc.print_list Misc.newline curveto fmt l
      end ;
      if p.S.cycle then PS.close_path fmt
  | S.Point _ ->
      failwith "Metapost fails in that case what should I do???"

let rec picture fmt = function
  | Empty -> ()
  | OnTop l ->
      Misc.print_list Misc.newline picture fmt l
  | Stroke_path(p,_,_,_) ->
      path fmt p
  | _ -> assert false
(*
      Cairo.save cr;
      color_option cr c;
      dash cr d;
      MetaPath.stroke cr pen path;
      Cairo.restore cr
*)
(*
  | Transform (m,t) ->
      Cairo.save cr;
      Cairo.transform cr m;
      (*Format.printf "Transform : %a@." Matrix.print m;*)
      draw_aux cr t;
      Cairo.restore cr
  | OnTop l -> List.iter (draw_aux cr) l
  | Tex t ->
      Cairo.save cr;
      let ({y=min},{y=max}) = Gentex.bounding_box t in
      inversey cr (max+.min);
      draw_tex cr t;
      Cairo.restore cr
  | Fill_path (path,c)->
      Cairo.save cr;
      color_option cr c;
      MetaPath.fill cr path;
      Cairo.restore cr
  | Clip (com,p) ->
      Cairo.save cr;
      MetaPath.draw_path cr p;
      Cairo.clip cr;
      draw_aux cr com;
      Cairo.restore cr
  | ExternalImage (filename,height,width) ->
      Cairo.save cr;
      inversey cr height;
      let img = Cairo_png.image_surface_create_from_file filename in
      let iwidth = float_of_int (Cairo.image_surface_get_width img) in
      let iheight = float_of_int (Cairo.image_surface_get_height img) in
      Cairo.scale cr (width/.iwidth) (height/.iheight);
      Cairo.set_source_surface cr img 0. 0.;
      Cairo.paint cr;
      Cairo.restore cr
*)

let draw fmt x =
  (* TODO specialize *)
  fprintf fmt "%%!PS@\n";
  fprintf fmt "%%%%BoundingBox: -1 -1 51 51@\n";
  fprintf fmt "%%%%HiResBoundingBox: -0.25 -0.25 50.25 50.25@\n";
  fprintf fmt "%%%%Creator: Mlpost 0.9@\n";
  fprintf fmt "%%%%CreationDate: 2010.04.12:0849@\n";
  fprintf fmt "%%%%Pages: 1@\n";
  fprintf fmt "%%%%BeginProlog@\n";
  fprintf fmt "%%%%EndProlog@\n";
  fprintf fmt "%%%%Page: 1 1@\n";
  picture fmt (content x)

let dump () =
  let _,fn,fig = Queue.pop Metapost.figures in
  let fig = LookForTeX.commandpic fig in
  let fn = fn ^ ".mps" in
  let c = open_out fn in
  let fmt = Format.formatter_of_out_channel c in
  draw fmt fig;
  Format.fprintf fmt "@.";
  close_out c

