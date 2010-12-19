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
let jobname = "gentex"
let filename = File.from_string jobname
(* FIXME different from Metapost.default_prelude? *)

let latex_cmd =
  Printf.sprintf "latex -jobname=%s -ipc -halt-on-error" jobname
(*   Printf.sprintf "cat" *)

type t = {tex   : Dviinterp.page;
          trans : Matrix.t;
          bb    : (float * float * float * float)}

let set_verbosity b = ()

type proc = { outc : in_channel; inc : out_channel; errc : in_channel }

type comm = { latex : proc ; dvi : Dvi.Incremental.t }

let truncate_or_touch f =
  File.open_in_gen [Open_trunc; Open_creat; Open_rdonly] 0o600 f

let ends_with en s =
  let l = String.length s in
  let k = String.length en in
  if k <= l then
    try
      for i = 0 to k - 1 do
        if s.[l - i - 1] <> en.[k - i - 1] then raise Exit
      done;
      true
    with Exit -> false
  else false


let read_up_to_one =
  (* FIXME deal with EOF exception: in this case, the user latex was probably
   * broken and latex stopped with an error *)
  let end_ = "[1]" in
  let rec aux inc =
    let s = input_line inc in
    print_endline s;
    if ends_with end_ s then () else aux inc in
  aux

let read_up_to_one p = read_up_to_one p.outc

let mk_proc_latex () =
  let outc,inc,errc =
    Unix.open_process_full latex_cmd (Unix.environment ()) in
  { outc = outc; inc = inc ; errc = errc }

let write_to_proc p s =
  Printf.fprintf p.inc s

let push_prelude p prel =
  (* the shipout macro from metapost, that adds a vrule at the end of the tex
   * to obtain easily the size of the dvi *)
  write_to_proc p
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
  \\ht0=0pt \\dp0=0pt \\box0 \\egroup}\n%!" prel

let shipout_and_flush p s =
  write_to_proc p "\\mpxshipout %s\\stopmpxshipout\n%!" s

let end_doc p =
  write_to_proc p "\\end{document}\n%!"

let extract cl =
  (* the vrule added by the metapost shipout macro is exploited and removed *)
  match cl with
    | Dviinterp.Fill_rect (_,x,y,_,h)::cl ->
      let bb = (0., -.(y+.h), x, -.y) in
      {tex = cl; trans = Matrix.identity; bb = bb}
    | _ -> assert false

let comm = ref None

let create tex =
  (* FIXME at some point we have to close the latex process *)
  match !comm with
  | None ->
      let p = mk_proc_latex () in
      push_prelude p (Defaults.get_prelude ());
      shipout_and_flush p tex;
      read_up_to_one p;
      let dvi_chan = File.open_in (File.set_ext filename "dvi") in
      let t, pgs = Dvi.Incremental.mk_t dvi_chan in
      comm := Some { latex = p ; dvi = t };
      extract (Dviinterp.Incremental.load_page t (List.hd pgs))
  | Some p ->
      shipout_and_flush p.latex tex;
      read_up_to_one p.latex;
      let pgs = Dvi.Incremental.next_pages p.dvi in
      extract (Dviinterp.Incremental.load_page p.dvi (List.hd pgs))

let point_of_cm cm = (0.3937 *. 72.) *. cm

let get_dimen_cm x = x.bb
let get_dimen_pt x =
  let (x_min,y_min,x_max,y_max) = get_dimen_cm x in
  (point_of_cm x_min,
   point_of_cm y_min,
   point_of_cm x_max,
   point_of_cm y_max)
(** donne la dimension en centimètre *)

let get_bases_cm x = assert false
let get_bases_pt x = assert false

let bounding_box x =
  let (xmin,ymin,xmax,ymax) = get_dimen_pt x in
  if Defaults.get_debug () then
    Format.printf "gentex bb : %f %f %f %f@." xmin ymin xmax ymax;
  {x=xmin;y=ymin},{x=xmax;y=ymax}

let print fmt tex =
  let min,max = bounding_box tex in
  Format.fprintf fmt "[%a,%a]" print min print max

let deb_print fmt tex =
  Format.printf "{ tex: %a ; matrix: %a }" Dviinterp.Print.page tex.tex
    Matrix.print tex.trans
