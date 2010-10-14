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
let default_prelude = "\\documentclass{article}\n"

let latex_cmd =
  Printf.sprintf "latex -jobname=%s -ipc -halt-on-error" jobname
(*   Printf.sprintf "cat" *)

let debug = false

type t = {tex   : Dviinterp.page;
          trans : Matrix.t;
          bb    : (float * float * float * float)}

let set_verbosity b = ()

type proc =
  { in_descr : Unix.file_descr ;
    out_descr : Unix.file_descr ;
    err_descr : Unix.file_descr;
    out_chan : in_channel;
    inc : out_channel;
(*
    dvi_channel : in_channel;
    dvi_descr : Unix.file_descr;
*)
  }

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

let rec read_up_to_one =
  let end_ = "[1]" in
  let rec aux inc =
    if ends_with end_ (input_line inc) then () else aux inc in
  aux

let read_up_to_one p = read_up_to_one p.out_chan

(*
let read_pages =
  let buf = String.create 1024 in
  let rec aux p =
    let can_read,_,_ =
      Unix.select [p.out_descr ; p.err_descr ; p.dvi_descr ] [] [] (-1.) in
    if List.mem p.out_descr can_read then begin
      let ret = Unix.read p.out_descr buf 0 1024 in
      if ret <> 0 then ignore (Unix.write Unix.stdout buf 0 ret);
      aux p
    end else if List.mem p.err_descr can_read then begin
      let ret = Unix.read p.err_descr buf 0 1024 in
      if ret <> 0 then ignore (Unix.write Unix.stderr buf 0 ret);
      aux p;
    end else if List.mem p.dvi_descr can_read then
      let pgs = Dvi.Incremental.next_pages p.dvi_data in
      if pgs = [] then aux p else pgs
      else aux p in
  aux
*)

let mk_proc_latex () =
  let outc,inc,errc =
    Unix.open_process_full latex_cmd (Unix.environment ()) in
  Printf.printf "created process: %s\n%!" latex_cmd;
  let out_descr = Unix.descr_of_in_channel outc and
      err_descr = Unix.descr_of_in_channel errc and
      in_descr = Unix.descr_of_out_channel inc in
(*   let dvi = Dvi.Incremental.mk_t dvi_chan in *)
  { in_descr = in_descr ; out_descr = out_descr ;
    err_descr = err_descr ; inc = inc ;
(*
    dvi_channel = dvi_chan;
    dvi_descr = dvi_descr;
*)
    out_chan = outc }

let write_and_wait p s =
(*   Printf.kfprintf (fun _ -> read_all p) p.cout s *)
  Printf.fprintf p.inc s

let push_prelude p prel =
  write_and_wait p
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
  write_and_wait p "\\mpxshipout %s\\stopmpxshipout\n%!" s

let end_doc p =
  write_and_wait p "\\end{document}\n%!"

let extract cl =
  (* remove the last rule added above, it gives the bounding box*)
  match cl with
    | Dviinterp.Fill_rect (_,x,y,_,h)::cl ->
      let bb = (0., -.(y+.h), x, -.y) in
      {tex = cl; trans = Matrix.identity; bb = bb}
    | _ -> assert false

let create prelude texs =
  match texs with
  | [] -> []
  | first::rest ->
      let p = mk_proc_latex () in
      let prelude =
        if prelude = "" then default_prelude else prelude in
      Printf.printf "prelude: %s\n%!" prelude;
      Printf.printf "%s\n%!" (Sys.getcwd ());
      Printf.printf "launched latex proc\n%!";
      push_prelude p prelude;
      Printf.printf "pushed prelude\n%!";
      shipout_and_flush p first;
      Printf.printf "pushed first page\n%!";
      read_up_to_one p;
      let dvi_chan = File.open_in (File.set_ext filename "dvi") in
      let t, pgs = Dvi.Incremental.mk_t dvi_chan in
      let texed = List.fold_left (fun acc tex ->
        shipout_and_flush p tex;
        read_up_to_one p;
        Printf.printf "shipped out: %s \n%!" tex;
        let l = Dvi.Incremental.next_pages t in
        let k = List.length l in
        Printf.printf "\n%!found: %d\n%!" k;
         l @ acc) pgs rest in
      end_doc p;
      let f = Dviinterp.Incremental.load_page t in
      List.rev_map (fun x -> extract (f x)) texed



(*
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
    let texfile = File.set_ext "tex" filename in
    let file = File.open_out texfile in
    Printf.fprintf file "%t" format;
    close_out file;
    let exit_status = Sys.command (sprintf "%s -halt-on-error %s > \
gentex_dev_null.log" com_latex (File.to_string texfile)) in
    if exit_status <> 0 then failwith (sprintf "Error with : %s : \
%s %s log in gentex.log" (File.Dir.to_string pwd) com_latex
  (File.to_string texfile));
    let dvi = File.set_ext filename "dvi"  in
    let saved = Dviinterp.load_file dvi in
    let extract cl =
      (* remove the last rule added above, it gives the bounding box*)
      match cl with
        | Dviinterp.Fill_rect (_,x,y,_,h)::cl ->
          let bb = (0., -.(y+.h), x, -.y) in
          {tex = cl; trans = Matrix.identity; bb = bb}
        | _ -> assert false in
    List.map extract saved, [] in
  Metapost_tool.tempdir "gentex" "" todo
*)

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
  if debug then
    Format.printf "gentex bb : %f %f %f %f@." xmin ymin xmax ymax;
  {x=xmin;y=ymin},{x=xmax;y=ymax}

let print fmt tex =
  let min,max = bounding_box tex in
  Format.fprintf fmt "[%a,%a]" print min print max

let deb_print fmt tex =
  Format.printf "{ tex: %a ; matrix: %a }" Dev_save.Print.page tex.tex
    Matrix.print tex.trans
