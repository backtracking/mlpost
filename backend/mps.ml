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

open Point_lib
open Matrix
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
  let stroke fmt = string fmt " stroke "
  let fill fmt = string fmt " fill "
end

let transform fmt t =
  fprintf fmt " [ %a %a %a %a %a %a] "
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

let option p fmt o =
  match o with
  | None -> ()
  | Some c -> p fmt c

let float fmt f = fprintf fmt "%g" f

let scolor fmt c =
  match c with
  | Concrete_types.RGB (r,g,b) ->
      fprintf fmt " %a %a %a setrgbcolor " float r float g float b
  | Concrete_types.CMYK (c,m,y,k) ->
      fprintf fmt " %a %a %a %a setcmykcolor " float c float m float y float k
  | Concrete_types.Gray c ->
      fprintf fmt " %a %a %a setrgbcolor " float c float c float c

let color fmt c =
  match c with
  | Concrete_types.OPAQUE c -> scolor fmt c
  | Concrete_types.TRANSPARENT _ -> assert false


let rec picture fmt = function
  | Empty -> ()
  | OnTop l ->
      Misc.print_list Misc.newline picture fmt l
  | Stroke_path(p,c,_,_) ->
      option color fmt c;
(*
      fprintf fmt "0 0.5 dtransform truncate idtransform
      setlinewidth pop \
       [3 3 ] 0 setdash 1 setlinecap 1 setlinejoin 10 setmiterlimit";
*)
      path fmt p; PS.stroke fmt
  | Fill_path (p,c)->
      option color fmt c;
      path fmt p;
      PS.fill fmt
(*
  | Tex t ->
      Cairo.save cr;
      let ({y=min},{y=max}) = Gentex.bounding_box t in
      inversey cr (max+.min);
      draw_tex cr t;
      Cairo.restore cr
*)
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
  picture fmt (content x);
  fprintf fmt "@\nshowpage@\n";
  fprintf fmt "%%%%EOF@\n"

let dump () =
  let _,fn,fig = Queue.peek Metapost.figures in
  let fig = LookForTeX.commandpic fig in
  let fn = fn ^ ".mps" in
  let c = open_out fn in
  let fmt = Format.formatter_of_out_channel c in
  draw fmt fig;
  Format.fprintf fmt "@.";
  close_out c

let generate_one fn fig =
  Misc.write_to_formatted_file fn (fun fmt -> draw fmt fig)

let generate bn ?(pdf=false) figs =
  let basename = Filename.basename bn in
  let suf = if pdf then ".mps" else ".1" in
  let sep = if pdf then "-" else "." in
  List.iter (fun (i,fig) ->
    let si = string_of_int i in
    let fn = basename ^ si ^ sep ^ suf in
    let fig = LookForTeX.commandpic fig in
    generate_one fn fig) figs

