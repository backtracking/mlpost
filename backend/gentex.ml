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

open Printf
open Point_lib
let com_latex = "latex"
let genfile_name = "gentex"
let default_prelude = "\\documentclass{article}\n"

let debug = false

let tempdir = Metapost_tool.tempdir

type t = {tex : Dev_save.t;
          trans : Matrix.t}

module Saved_device = Dviinterp.Interp(Dev_save.Dev_save)

let set_verbosity b =
  Saved_device.set_verbosity b

let create prelude = function
  | [] -> []
  | texs ->
  let format fmt = Printf.fprintf fmt
    "%s
\\begin{document}
\\gdef\\mpxshipout{\\shipout\\hbox\\bgroup%%
  \\setbox0=\\hbox\\bgroup}%%
\\gdef\\stopmpxshipout{\\egroup  \\dimen0=\\ht0 \\advance\\dimen0\\dp0
  \\dimen1=\\ht0 \\dimen2=\\dp0
  \\setbox0=\\hbox\\bgroup
    \\box0
    \\ifnum\\dimen0>0 \\vrule width1sp height\\dimen1 depth\\dimen2
    \\else \\vrule width1sp height1sp depth0sp\\relax
    \\fi\\egroup
  \\ht0=0pt \\dp0=0pt \\box0 \\egroup}
%a
\\end{document}" (if prelude = "" then default_prelude else prelude)
(fun fmt -> List.iter (Printf.fprintf fmt
"\\mpxshipout
%s\\stopmpxshipout")) texs in
  let todo _ pwd =
    let latex = genfile_name^".tex" in
    let file = open_out latex in
    Printf.fprintf file "%t" format;
    close_out file;
    let exit_status = Sys.command (sprintf "%s -halt-on-error %s > \
gentex_dev_null.log" com_latex latex) in
    if exit_status <> 0 then failwith (sprintf "Error with : %s : \
%s %s log in gentex.log" (File.Dir.to_string pwd) com_latex latex);
    let dvi = genfile_name^".dvi" in
    let saved = Saved_device.load_file true dvi in
    List.map (fun x -> {tex = x;trans= Matrix.identity})
      (Dev_save.separate_pages saved), [] in
  tempdir genfile_name "" todo

let point_of_cm cm = (0.3937 *. 72.) *. cm

let get_dimen_cm x = Dev_save.get_dimen_first_page x.tex
let get_dimen_pt x =
  let (x_min,y_min,x_max,y_max) = get_dimen_cm x in
  (point_of_cm x_min,
   point_of_cm y_min,
   point_of_cm x_max,
   point_of_cm y_max)
(** donne la dimension en centimètre *)

let get_bases_cm x = Dev_save.get_bases_first_page x.tex
let get_bases_pt x = List.map point_of_cm (get_bases_cm x)

let bounding_box x =
  let (xmin,ymin,xmax,ymax) = get_dimen_pt x in
  if debug then
    Format.printf "gentex bb : %f %f %f %f@." xmin ymin xmax ymax;
  {x=xmin;y=ymin},{x=xmax;y=ymax}

let print fmt tex =
  let min,max = bounding_box tex in
  Format.fprintf fmt "[%a,%a]" print min print max

let deb_print fmt tex =
  Format.printf "{ tex: %a ; matrix: %a }" Dev_save.Print.dvi tex.tex
    Matrix.print tex.trans
