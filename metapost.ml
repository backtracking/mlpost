(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Johannes Kanig, Stephane Lescuyer                       *)
(*  and Jean-Christophe Filliatre                                         *)
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

open Format
open Misc
open Types
module C = Compiled_types

let print_name = pp_print_string

let print_corner fmt = function
  | N -> fprintf fmt "n"
  | S -> fprintf fmt "s"
  | W -> fprintf fmt "w"
  | E -> fprintf fmt "e"
  | NW -> fprintf fmt "nw"
  | NE -> fprintf fmt "ne"
  | SW -> fprintf fmt "sw"
  | SE -> fprintf fmt "se"

let print_piccorner fmt = function
  | UL -> fprintf fmt "ulcorner"
  | LL -> fprintf fmt "llcorner"
  | UR -> fprintf fmt "urcorner"
  | LR -> fprintf fmt "lrcorner"

let print_position fmt = function
  | Pcenter  -> fprintf fmt ""
  | Pleft   -> fprintf fmt ".lft"
  | Pright  -> fprintf fmt ".rt"
  | Ptop    -> fprintf fmt ".top"
  | Pbot    -> fprintf fmt ".bot"
  | Pupleft  -> fprintf fmt ".ulft"
  | Pupright -> fprintf fmt ".urt"
  | Plowleft -> fprintf fmt ".llft"
  | Plowright -> fprintf fmt ".lrt"

let rec print_num fmt = function
  | C.F f ->
      if f = infinity then fprintf fmt "infinity"
      else if f > 4095. then fprintf fmt "%g" 4095.
      else if abs_float f < 0.0001 then fprintf fmt "0"
      else fprintf fmt "%g" f
  | C.NXPart p -> fprintf fmt "xpart %a" print_point p
  | C.NYPart p -> fprintf fmt "ypart %a" print_point p
  | C.NAdd (f1, f2) -> fprintf fmt "(%a +@ %a)" print_num f1 print_num f2
  | C.NSub (f1, f2) -> fprintf fmt "(%a -@ %a)" print_num f1 print_num f2
  | C.NMult (f1, f2) -> fprintf fmt "(%a *@ %a)" print_num f1 print_num f2
  | C.NDiv (f1, f2) -> fprintf fmt "(%a /@ %a)" print_num f1 print_num f2
  | C.NMax (f1, f2) -> fprintf fmt "max(@ %a,@ %a)" print_num f1 print_num f2
  | C.NMin (f1, f2) -> fprintf fmt "min(@ %a,@ %a)" print_num f1 print_num f2
  | C.NGMean (f1, f2) -> fprintf fmt "(%a@ ++@ %a)" print_num f1 print_num f2
  | C.NName n -> pp_print_string fmt n
  | C.NLength p -> fprintf fmt "length (%a)" print_path p

and print_float fmt f = print_num fmt (C.F f)

and print_color fmt = function
  | RGB (r,g,b) -> 
      fprintf fmt "(%a, %a , %a)" print_float r print_float g print_float b
  | CMYK (c,m,y,k) ->
      fprintf fmt "(%a, %a, %a, %a)" print_float c print_float m 
        print_float y print_float k
  | Gray f -> fprintf fmt "%a * white" print_float f

and print_point fmt = function
  | C.PTPair (m,n) -> fprintf fmt "(%a,%a)" print_num m print_num n
  | C.PTPicCorner (pic, d) -> 
      fprintf fmt "(%a %a)" print_piccorner d print_picture pic
  | C.PTAdd (p1, p2) -> fprintf fmt "(%a + %a)" print_point p1 print_point p2
  | C.PTSub (p1, p2) -> fprintf fmt "(%a - %a)" print_point p1 print_point p2
  | C.PTMult (f, p) -> fprintf fmt "(%a * %a)" print_num f print_point p
  | C.PTRotated (f, p) ->  
      fprintf fmt "(%a rotated %a)" print_point p print_float f
  | C.PTPointOf (f, p) ->
      fprintf fmt "(point %a of (%a))" print_float f print_path p
  | C.PTDirectionOf (f, p) ->
      fprintf fmt "(direction %a of (%a))" print_float f print_path p
  | C.PTTransformed (p,tr) -> fprintf fmt "((%a) %a)"
      print_point p print_transform_list (List.rev tr)
  | C.PTName pn -> pp_print_string fmt pn

and print_transform fmt = function
  | C.TRScaled a -> fprintf fmt "scaled %a" print_num a
  | C.TRShifted a -> fprintf fmt "shifted %a" print_point a
  | C.TRRotated a -> fprintf fmt "rotated %a" print_float a
  | C.TRSlanted a -> fprintf fmt "slanted %a" print_num a
  | C.TRXscaled a -> fprintf fmt "xscaled %a" print_num a
  | C.TRYscaled a -> fprintf fmt "yscaled %a" print_num a
  | C.TRZscaled a -> fprintf fmt "zscaled %a" print_point a
  | C.TRReflect (p1,p2) -> 
      fprintf fmt "reflectedabout (%a,%a)" 
        print_point p1 print_point p2
  | C.TRRotateAround (p,f) ->
      fprintf fmt "rotatedaround(%a,%a)"
        print_point p print_float f

and print_transform_list fmt l = Misc.print_list space print_transform fmt l

and print_picture fmt = function
  | C.PITex s -> fprintf fmt "btex %s etex" s
  | C.PITransform (tr, p) ->
      fprintf fmt "((%a) %a)" print_picture p print_transform_list tr
  | C.PIName n -> pp_print_string fmt n

and print_path fmt = function
  | C.PAFullCircle -> fprintf fmt "fullcircle"
  | C.PAHalfCircle -> fprintf fmt "halfcircle"
  | C.PAQuarterCircle -> fprintf fmt "quartercircle"
  | C.PAUnitSquare -> fprintf fmt "unitsquare"
  | C.PATransformed (p,tr) -> fprintf fmt "((%a) %a)"
      print_path p print_transform_list tr
  | C.PAAppend (p1,j,p2) -> 
      fprintf fmt "%a %a@ %a" print_path p1 print_joint j print_path p2
  | C.PACycle (d,j,p) ->
      fprintf fmt "%a %a %acycle" print_path p print_joint j print_dir d
  | C.PAConcat (k,j,p) ->
      fprintf fmt "%a %a@ %a" print_path p print_joint j print_knot k
  | C.PAKnot k -> print_knot fmt k
  | C.PACutAfter (p1, p2) -> 
      fprintf fmt "%a cutafter %a@ " print_path p2 print_path p1
  | C.PACutBefore (p1, p2) -> 
      fprintf fmt "%a cutbefore %a@ " print_path p2 print_path p1
  | C.PABuildCycle l ->
      fprintf fmt "buildcycle(%a)" 
        (Misc.print_list comma print_path) l
  | C.PASub (f1, f2, p) ->
      fprintf fmt "subpath(%a,%a) of %a" 
	print_float f1 print_float f2 print_name p
  | C.PABBox p -> fprintf fmt "bbox %a" print_picture p
  | C.PAName n -> pp_print_string fmt n

and print_joint fmt = function
  | C.JLine -> fprintf fmt "--"
  | C.JCurve -> fprintf fmt ".."
  | C.JCurveNoInflex -> fprintf fmt "..."
  | C.JTension (a,b) -> 
      fprintf fmt "..tension %a and %a .." print_float a print_float b
  | C.JControls (a,b) -> 
      fprintf fmt "..controls %a and %a .." print_point a print_point b

and print_dir fmt = function
  | C.NoDir -> ()
  | C.Vec p -> fprintf fmt "{%a}" print_point p
  | C.Curl f -> fprintf fmt "{curl %a}" print_float f

and print_knot fmt (d1,p,d2) = 
  fprintf fmt "%a%a%a" print_dir d1 print_point p print_dir d2

and print_dash fmt = function
  | C.DEvenly -> fprintf fmt "evenly"
  | C.DWithdots -> fprintf fmt "withdots"
  | C.DScaled (s, d) -> fprintf fmt "%a scaled %a" print_dash d print_float s
  | C.DShifted (p, d) -> fprintf fmt "%a shifted %a" print_dash d print_point p
  | C.DPattern l -> 
      fprintf fmt "dashpattern(";
      List.iter 
	(fun p -> 
	  let p,n = match p with C.On n -> "on", n | C.Off n -> "off", n in
	    fprintf fmt "%s %a " p print_num n) 
	l;
      fprintf fmt ")" 

and print_pen fmt = function
  | C.PenCircle -> fprintf fmt "pencircle"
  | C.PenSquare -> fprintf fmt "pensquare"
  | C.PenFromPath p -> fprintf fmt "makepen (%a)" print_path p
  | C.PenTransformed (p,tr) -> 
      fprintf fmt "%a %a" print_pen p print_transform_list tr

and print_command fmt  = function
  | C.CDraw (path, color, pen, dashed) ->
      fprintf fmt "@[<hov 2>draw@ %a@,%a@,%a@,%a;@]@\n" print_path path
        (print_option " withcolor " print_color) color
        (print_option " withpen " print_pen) pen
        (print_option " dashed " print_dash) dashed
  | C.CDrawArrow (path, color, pen, dashed) ->
      fprintf fmt "drawarrow %a%a%a%a;@\n" print_path path
        (print_option " withcolor " print_color) color
        (print_option " withpen " print_pen) pen
        (print_option " dashed " print_dash) dashed
  | C.CFill (path, color) ->
      fprintf fmt "fill %a%a;@\n" print_path path
        (print_option " withcolor " print_color) color
  | C.CLabel (pic,pos,p) ->
      fprintf fmt "label%a(%a,@ %a); @\n"
        print_position pos print_picture pic print_point p
  | C.CDotLabel (pic,pos,p) ->
      fprintf fmt "@[<hov 2>dotlabel%a(%a,@ %a);@]@\n"
        print_position pos print_picture pic print_point p
  | C.CDrawPic p ->
      fprintf fmt "draw %a;@\n" print_picture p
  | C.CSeq l -> List.iter (fun c -> print_command fmt c) l
  | C.CDeclPath (n, p) ->
      fprintf fmt "path %s ;@\n%s = %a;@\n" n n print_path p
  | C.CDeclPoint (n, p) ->
      fprintf fmt "pair %s ;@\n%s = %a;@\n" n n print_point p
  | C.CDeclNum (n, nm) ->
      fprintf fmt "numeric %s ;@\n%s = %a;@\n" n n print_num nm
  | C.CSimplePic (pn1, pexpr) ->
      fprintf fmt "picture %s;@\n" pn1;
      fprintf fmt "%s := %a;@\n" pn1 print_picture pexpr;
  | C.CDefPic (pic, cmd) ->
      (* Declpic (savepic, currentpicture);
       * Assign (currentpicture, nullpicture);
       * cmd;
       * Assign (pic, currentpicture);
       * Assign (currentpicture, savepic) *)
      let savepic = Name.picture () in
      fprintf fmt "picture %s, %s ;@\n" savepic pic;
      fprintf fmt "%s = currentpicture;@\n" savepic;
      fprintf fmt "currentpicture := nullpicture;@\n";
      print_command fmt cmd;
      fprintf fmt "%s = currentpicture;@\n" pic;
      fprintf fmt "currentpicture := %s;@\n" savepic
  | C.CClip (pic,pth) ->
      fprintf fmt "clip %s to %a;@\n" pic print_path pth

let print i fmt l =
  (* resetting is actually not needed; variables other than 
     x,y are not local to figures *)
(*   Compile.reset (); *)
  let l = List.map Newcompile.command l in
  fprintf fmt "@[beginfig(%d)@\n  @[%a@] endfig;@]@." i 
    (fun fmt l -> List.iter (print_command fmt) l)
    l

let print_prelude ?(eps=false) s fmt () =
  if eps then 
    fprintf fmt "prologues := 2;@\n";
  fprintf fmt "verbatimtex@\n";
  fprintf fmt "%%&latex@\n";
  fprintf fmt "%s" s;
  fprintf fmt "\\begin{document}@\n";
  fprintf fmt "etex@\n"
  (* fprintf fmt "input boxes;@\n" *)

let defaultprelude = "\\documentclass{article}\n\\usepackage[T1]{fontenc}\n"

let generate_mp fn ?(prelude=defaultprelude) ?eps l =
  Misc.write_to_formatted_file fn
    (fun fmt -> 
       print_prelude ?eps prelude fmt ();
       List.iter (fun (i,f) -> print i fmt f) l)

(* batch processing *)

let figuren = ref 0
let figures = Queue.create ()

let emit s f = 
  incr figuren;
  Queue.add (!figuren, s, f) figures

let dump_tex ?prelude f =
  let c = open_out (f ^ ".tex") in
  let fmt = formatter_of_out_channel c in
  begin match prelude with
    | None -> 
	fprintf fmt "\\documentclass[a4paper]{article}";
	fprintf fmt "\\usepackage{graphicx}"
    | Some s ->
	fprintf fmt "%s@\n" s
  end;
  fprintf fmt "\\begin{document}@\n";
  fprintf fmt "\\begin{center}@\n";
  Queue.iter
    (fun (_,s,_) ->
       fprintf fmt "\\hrulefill\\verb!%s!\\hrulefill\\\\[1em]@\n" s;
       fprintf fmt "\\framebox{\\includegraphics[width=\\textwidth]{%s.mps}}\\\\[1em]@\n" s;
       fprintf fmt "\\framebox{\\includegraphics{%s.mps}}\\\\@\n" s;
       fprintf fmt "\\hrulefill\\\\@\n@\n\\medskip@\n";)
    figures;
  fprintf fmt "\\end{center}@\n";
  fprintf fmt "\\end{document}@.";
  close_out c

let dump ?prelude ?(pdf=false) ?eps bn = 
  let f = bn ^ ".mp" in
  let figl = Queue.fold (fun l (i,_,f) -> (i,f) :: l) [] figures in
  generate_mp f ?prelude ?eps figl;
  let out = Sys.command (sprintf "mpost %s end" f) in
  if out <> 0 then exit 1;
  let suf = if pdf then ".mps" else ".1" in
  Queue.iter 
    (fun (i,s,_) -> 
      Sys.rename (bn ^ "." ^ string_of_int i) (s ^ suf))
    figures

let slideshow l k = 
  let l = List.map (fun f -> Picture.make (Command.seq f)) l in
  let l' = Command.seq (List.map 
                  (fun p -> Command.draw 
                              ~color:Color.white 
                              (Picture.bbox p)) l)
  in
  let x = ref (k-1) in
    List.map (fun p -> 
                  incr x;
                  !x, [Command.append l' (Command.draw_pic p)]) l


