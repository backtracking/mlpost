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

open Tfm
open Fonts
open Dviinterp

type command = Dviinterp.command

type t = command list list
type page = command list

type dimen = { mutable tx_min : float;
               mutable ty_min : float;
               mutable tx_max : float;
               mutable ty_max : float}

let get_dimen s = function
  | Fill_rect (info,x,y,w,h) ->
    let xmin,xmax = x,x+.w(*min x (x+.w), max x (x+.w)*) in
    let ymin,ymax = y,y+.h(*min y (y+.h), max y (y+.h)*) in
    s.tx_min <- (min s.tx_min xmin);
    s.ty_min <- (min s.ty_min ymin);
    s.tx_max <- (max s.tx_max xmax);
    s.ty_max <- (max s.ty_max ymax)
  | Draw_text text ->
    let char c =
      let width,height,depth =
        Fonts.char_dims text.tex_font (Int32.to_int c) in
      Format.printf "w=%f h=%f d=%f@." width height depth;
      let (x,y) = text.tex_pos in
      s.tx_min <- min s.tx_min x;
      s.ty_min <- min s.ty_min (y-.depth);
      s.tx_max <- max s.tx_max (x+.width);
      s.ty_max <- max s.ty_max (y+.height) in
    List.iter char text.tex_string
  | Specials _ -> ()
  | Draw_text_type1 _ -> assert false

let get_dimen p =
  let dimen = { tx_min = infinity;
                tx_max = neg_infinity;
                ty_min = infinity;
                ty_max = neg_infinity} in
  List.iter (get_dimen dimen) p;
  (dimen.tx_min,dimen.tx_max,dimen.ty_min,dimen.ty_max)

module Print = struct
  (* debug printing *)
  open Format
  let command fmt c =
    match c with
    | Fill_rect (_,x,y,w,h) -> fprintf fmt "rect(%f,%f,%f,%f)" x y w h
    | Draw_text text -> fprintf fmt "glyph (%s)" (Fonts.tex_name text.tex_font)
    | Specials _ -> assert false
    | Draw_text_type1 _ -> assert false

  let page fmt p =
    fprintf fmt "[< %a >]" (Misc.print_list Misc.newline command) p

  let dvi fmt d =
    Misc.print_list (fun fmt () ->
      fprintf fmt "<newpage>@\n@\n") page fmt d
end
