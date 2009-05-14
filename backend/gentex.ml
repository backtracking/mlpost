open Printf
open Point_lib
let com_latex = "latex"
let genfile_name = "gentex"
let default_prelude = "\\documentclass{article}\n\\begin{document}"

let _ = Random.self_init ()

let debug = false


let tempdir prefix suffix f =
  let rec create_dir () =
    try
      let dirname = Filename.concat Filename.temp_dir_name (Printf.sprintf "%s%i%s" prefix (Random.int 10000) suffix) in 
      Unix.mkdir dirname  0o700; dirname
    with | Unix.Unix_error (Unix.EEXIST, _, _) -> create_dir () in
  let dirname = create_dir () in
  let workdir_bak = Sys.getcwd () in
  Sys.chdir dirname;
  let res = f () in
  Sys.chdir workdir_bak;
  Array.iter (fun x -> Sys.remove (Filename.concat dirname x)) (Sys.readdir dirname);
  Unix.rmdir dirname;
  res
  

type t = {tex : Dev_save.t;
          trans : Matrix.t}

module Saved_device = Dviinterp.Interp(Dev_save.Dev_save)
module Cairo_device = Dev_save.Dev_load(Dvicairo.Cairo_device)

let create prelude = function
  | [] -> []
  | texs ->
  let format fmt = Printf.fprintf fmt
    "%s
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
  let todo () = 
    let latex = genfile_name^".tex" in
    let file = open_out latex in
    Printf.fprintf file "%t" format;
    close_out file;
    let exit_status = Sys.command (sprintf "%s %s > /dev/null" com_latex latex) in
    if exit_status <> 0 then failwith (sprintf "Error with : %s %s" com_latex latex);
    let dvi = genfile_name^".dvi" in
    let saved = Saved_device.load_file true dvi in
    List.map (fun x -> {tex = x;trans= Matrix.identity}) (Dev_save.separe_pages saved) in
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

let draw cr tex = 
  Cairo.save cr;
  Cairo.transform cr (Matrix.to_cairo tex.trans);
  Cairo_device.replay false tex.tex 
  {Dvicairo.pic = cr;new_page = (fun () -> assert false);
   x_origin = 0.; y_origin = 0.};
  Cairo.restore cr
  (*;Format.printf "Gentex : %a@." print tex*)
