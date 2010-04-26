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

let conversion = 0.3937 *. 72.
let point_of_cm cm = conversion *. cm

let float fmt f =
    (* PDF does not understand e notation, so we protect the printf which
    uses %g in the cases where this would use e notation; we do not need that
    much precision anyway*)
  if abs_float f < 0.0001 then fprintf fmt "0@ "
  else if abs_float f >= 1.e04 then
    fprintf fmt "%.4f@ " f
  else
    fprintf fmt "%.4g@ " f
module MPS = struct

  type line_cap =
    | ButtCap
    | RoundCap
    | SquareCap

  type line_join =
    | MiterJoin
    | RoundJoin
    | BevelJoin

  let psstart fmt = fprintf fmt "@["
  let psend fmt = fprintf fmt "@]@ "
  let string = pp_print_string
  let moveto_float fmt x y =
    fprintf fmt "%t%a%amoveto%t" psstart float x float y psend
  let lineto_float fmt x y =
    psstart fmt; float fmt x; float fmt y; string fmt "lineto"; psend fmt
(*     fprintf fmt "%t%a%alineto%t" psstart float x float y psend *)
  let moveto fmt p = moveto_float fmt p.x p.y
  let curveto fmt p1 p2 p3 =
    fprintf fmt "%t%f@ %f@ %f@ %f@ %f@ %f@ curveto%t"
      psstart p1.x p1.y p2.x p2.y p3.x p3.y psend

  let rlineto fmt p =
    fprintf fmt "%t%a%arlineto%t" psstart float p.x float p.y psend
  let close_path fmt = fprintf fmt "%tclose_path%t" psstart psend
  let newpath fmt = fprintf fmt "%tnewpath%t" psstart psend
  let stroke fmt = fprintf fmt "%tstroke%t" psstart psend
  let fill fmt = fprintf fmt "%tfill%t" psstart psend
  let showpage fmt = fprintf fmt "%tshowpage%t" psstart psend
  let clip fmt = fprintf fmt "%tclip%t" psstart psend

  let gsave fmt = fprintf fmt "%tgsave%t" psstart psend
  let grestore fmt = fprintf fmt "%tgrestore%t" psstart psend

  let setlinewidth fmt f =
   fprintf fmt "%t0@ %adtransform@ truncate@ idtransform@ setlinewidth@ pop%t"
     psstart float f psend

  let setlinecap fmt c =
    let i =
      match c with
      | ButtCap -> 0
      | RoundCap -> 1
      | SquareCap -> 2 in
    fprintf fmt "%t%d setlinecap%t" psstart i psend

  let setlinejoin fmt j =
    let i =
      match j with
      | MiterJoin -> 0
      | RoundJoin -> 1
      | BevelJoin -> 2 in
    fprintf fmt "%t%d setlinejoin%t" psstart i psend

  let matrix fmt t =
    fprintf fmt "%t[%a%a%a%a%a%a]%t" psstart
      float t.xx float t.yx float t.xy float t.yy float t.x0 float t.y0 psend

  let transform fmt t =
    fprintf fmt "%t%a concat%t" psstart matrix t psend

  let scolor fmt c =
    match c with
    | Concrete_types.RGB (r,g,b) ->
        fprintf fmt "%t%a%a%asetrgbcolor%t"
          psstart float r float g float b psend
    | Concrete_types.CMYK (c,m,y,k) ->
        fprintf fmt "%t%a%a%a%asetcmykcolor%t"
          psstart float c float m float y float k psend
    | Concrete_types.Gray c ->
        fprintf fmt "%t%asetgray%t" psstart float c psend

  let color fmt c =
    match c with
    | Concrete_types.OPAQUE c -> scolor fmt c
    | Concrete_types.TRANSPARENT _ -> assert false

  let char_const fmt c =
    fprintf fmt "%t(\\%03o)%t" psstart c psend

  let glyph fmt c font =
    fprintf fmt "%t%a%s@ %afshow%t" psstart char_const c
      (Fonts.tex_name font) float (Fonts.scale font conversion) psend

  let rectangle fmt p w h =
    newpath fmt;
    moveto fmt p;
    lineto_float fmt (p.x+.w) p.y;
    lineto_float fmt (p.x+.w) (p.y+.h);
    lineto_float fmt p.x (p.y+.h);
    close_path fmt;
    fill fmt
end

let in_context fmt f =
  MPS.gsave fmt;
  f ();
  MPS.grestore fmt

module MPS_device =
struct
  type arg =
    { fmt : Format.formatter;
      trans : Matrix.t }
  type t = arg

  type cooked = unit
  let new_document arg _ = arg
  let new_page _ = ()
  let fill_rect t _ x y w h =
    let x = point_of_cm x and y = point_of_cm y
    and w = point_of_cm w and h = point_of_cm h in
    let p = { x = x ; y = y } in
    let fmt = t.fmt in
    in_context fmt (fun () ->
      MPS.transform fmt t.trans;
      MPS.rectangle t.fmt p w h
    )

  let specials _ = assert false
  let end_document _ = ()

  let draw_char t _ font c f1 f2 =
    let f1 = point_of_cm f1 and f2 = point_of_cm f2 in
    let p = { x = f1; y = f2 } in
    let fmt = t.fmt in
    in_context fmt (fun () ->
      MPS.transform fmt t.trans;
      MPS.moveto fmt p;
      MPS.glyph fmt (Int32.to_int c) font
    )

end

module MyDevice = Dev_save.Dev_load(MPS_device)

let curveto fmt s =
  let _, sb, sc, sd = Spline.explode s in
  MPS.curveto fmt sb sc sd

let path =
  let rec path fmt = function
    | S.Path p ->
        begin match p.S.pl with
        | [] -> assert false
        | (x::_) as l ->
          MPS.moveto fmt (Spline.left_point x);
          Misc.print_list Misc.space curveto fmt l
        end ;
        if p.S.cycle then MPS.close_path fmt
    | S.Point p ->
        MPS.newpath fmt;
        MPS.moveto fmt p;
        MPS.rlineto fmt p in
  fun fmt p ->
    MPS.newpath fmt;
    path fmt p


let option p fmt o =
  match o with
  | None -> ()
  | Some c -> p fmt c

let pen fmt t =
  (* FIXME do something better *)
  (* for now assume that the pen is simply a scaled circle, so just grab the xx
   * value of the matrix and use that as linewidth *)
  MPS.setlinewidth fmt t.xx

let draw_tex fmt t =
  (* FIXME currently the transformation is applied and restored for every letter
   * *)
  let tr = t.Gentex.trans in
    MyDevice.replay false t.Gentex.tex { MPS_device.fmt = fmt ; trans = tr }


let rec picture fmt = function
  | Empty -> ()
  | OnTop l ->
      Misc.print_list Misc.space picture fmt l
  | Stroke_path(pa,clr,pe,_) ->
      (* FIXME dash pattern *)
      in_context fmt (fun () ->
        option MPS.color fmt clr;
        pen fmt pe;
        path fmt pa;
        MPS.stroke fmt)
  | Fill_path (p,c)->
      in_context fmt (fun () ->
        option MPS.color fmt c;
        path fmt p;
        MPS.fill fmt)
  | Tex t -> draw_tex fmt t
  | Transform (m,t) -> picture fmt (apply_transform_cmds m t)
  | Clip (com,p) ->
      in_context fmt (fun () ->
        path fmt p;
        MPS.clip fmt;
        picture fmt com
      )
  | _ -> assert false
(*
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
  let {x = minx; y = miny},{x = maxx; y = maxy} = Picture_lib.bounding_box x in
  let minxt, minyt, maxxt, maxyt =
    floor minx, floor miny, ceil maxx, ceil maxy in
  fprintf fmt "%%!PS@\n";
  fprintf fmt "%%%%BoundingBox: %f %f %f %f@\n" minxt minyt maxxt maxyt;
  fprintf fmt "%%%%HiResBoundingBox: %f %f %f %f@\n" minx miny maxx maxy;
  fprintf fmt "%%%%Creator: Mlpost %s@\n" Version.version;
  (* FIXME font declarations *)
  (* FIXME Date *)
  fprintf fmt "%%%%Pages: 1@\n";
  fprintf fmt "%%%%BeginProlog@\n";
  fprintf fmt "%%%%EndProlog@\n";
  fprintf fmt "%%%%Page: 1 1@\n";
  MPS.setlinewidth fmt (default_line_size /.2.);
  MPS.setlinecap fmt MPS.RoundCap;
  MPS.setlinejoin fmt MPS.RoundJoin;
  picture fmt (content x);
  MPS.showpage fmt;
  fprintf fmt "%%%%EOF@\n"

let generate_one fn fig =
  Misc.write_to_formatted_file fn (fun fmt -> draw fmt fig)

let dump () =
  Queue.iter (fun (_,fn,fig) ->
    let fig = LookForTeX.commandpic fig in
    let fn = fn ^ ".mps" in
    Misc.write_to_formatted_file fn (fun fmt -> draw fmt fig)
  ) Metapost.figures

let generate bn ?(pdf=false) figs =
  let basename = Filename.basename bn in
  let suf = if pdf then ".mps" else ".1" in
  let sep = if pdf then "-" else "." in
  List.iter (fun (i,fig) ->
    let si = string_of_int i in
    let fn = basename ^ sep ^ si ^ suf in
    let fig = LookForTeX.commandpic fig in
    generate_one fn fig) figs

