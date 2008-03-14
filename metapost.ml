(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Johannes Kanig, Stephane Lescuyer                       *)
(*  and Jean-Christophe Filliatre                                         *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2, with the special exception on linking              *)
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

let rec print_num fmt f =
  if f = infinity then fprintf fmt "infinity"
  else
    if f > 4095. then fprintf fmt "%4f" 4095.
    else fprintf fmt "%.4f" f

let print_float fmt f = print_num fmt f

let print_color fmt (r,g,b) =
  fprintf fmt "(%a, %a , %a)" print_float r print_float g print_float b

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

let rec print_point fmt = function
  | PTPair (m,n) -> fprintf fmt "(%a,%a)" print_num m print_num n
  | PTBoxCorner (n, d) -> fprintf fmt "%a.%a" print_name n print_corner d
  | PTAdd (p1, p2) -> fprintf fmt "(%a + %a)" print_point p1 print_point p2
  | PTSub (p1, p2) -> fprintf fmt "(%a - %a)" print_point p1 print_point p2
  | PTMult (f, p) -> fprintf fmt "(%a * %a)" print_float f print_point p
  | PTRotated (f, p) ->  
      fprintf fmt "(%a rotated %a)" print_point p print_float f
  | PTPointOf (f, p) ->
      fprintf fmt "(point %a of (%a))" print_float f print_path p

and print_transform fmt = function
  | TRScaled a -> fprintf fmt "scaled %a" print_num a
  | TRShifted a -> fprintf fmt "shifted %a" print_point a
  | TRRotated a -> fprintf fmt "rotated %a" print_float a
  | TRSlanted a -> fprintf fmt "slanted %a" print_num a
  | TRXscaled a -> fprintf fmt "xscaled %a" print_num a
  | TRYscaled a -> fprintf fmt "yscaled %a" print_num a
  | TRZscaled a -> fprintf fmt "zscaled %a" print_point a
  | TRReflect (p1,p2) -> 
      fprintf fmt "reflectedabout (%a,%a)" 
        print_point p1 print_point p2
  | TRRotateAround (p,f) ->
      fprintf fmt "rotatedaround(%a,%a)"
        print_point p print_float f

and print_transform_list fmt l =
  Misc.print_list space print_transform fmt l

and print_picture fmt = function
  | PITex s -> fprintf fmt "btex %s etex" s
  | PIMake _ -> assert false
  | PITransform (tr, p) -> 
      fprintf fmt "(%a transformed (identity %a))" 
	print_picture p print_transform_list tr
  | PIName n ->
      pp_print_string fmt n

and declare_box fmt = function
  | BCircle (n, c, p, s) -> 
      fprintf fmt "circleit.%a(%a);" print_name n print_picture p;
      fprintf fmt "%a.c = %a;@\n" print_name n print_point c;
      begin match s with
	| None -> ()
	| Some (Padding (dx, dy)) -> 
	    fprintf fmt "%a.dx = %a; %a.dy = %a;@\n" 
	      print_name n print_num dx print_name n print_num dy
	| Some (Ratio r) ->
	    fprintf fmt "%a.dx = %f * %a.dy;@\n" print_name n r print_name n
      end
  | BRect (n, c, p) -> 
      fprintf fmt "boxit.%a(%a);" print_name n print_picture p;
      fprintf fmt "%a.c = %a;@\n" print_name n print_point c

and print_path fmt = function
  | PAFullCircle -> fprintf fmt "fullcircle"
  | PAHalfCircle -> fprintf fmt "halfcircle"
  | PAQuarterCircle -> fprintf fmt "quartercircle"
  | PAUnitSquare -> fprintf fmt "unitsquare"
  | PATransformed (p,tr) -> fprintf fmt "((%a) %a)"
      print_path p print_transform_list tr
  | PAAppend (p1,j,p2) -> 
      fprintf fmt "%a %a@ %a" print_path p1 print_joint j print_path p2
  | PACycle (d,j,p) ->
      fprintf fmt "%a %a %acycle" print_path p print_joint j print_dir d
  | PAConcat (k,j,p) ->
      fprintf fmt "%a %a@ %a" print_path p print_joint j print_knot k
  | PAKnot k -> print_knot fmt k
  | PABoxBPath (BCircle (n, _, _, _) | BRect (n, _, _)) ->
      fprintf fmt "bpath.%a" print_name n
  | PACutAfter (p1, p2) -> 
      fprintf fmt "%a cutafter %a@ " print_path p2 print_path p1
  | PACutBefore (p1, p2) -> 
      fprintf fmt "%a cutbefore %a@ " print_path p2 print_path p1
  | PABuildCycle l ->
      fprintf fmt "buildcycle(%a)" 
        (Misc.print_list comma print_path) l
  | PASub (f1, f2, p) ->
      fprintf fmt "subpath(%a,%a) of %a" 
	print_float f1 print_float f2 print_path p
  | PAName n ->
      pp_print_string fmt n

and print_joint fmt = function
  | JLine -> fprintf fmt "--"
  | JCurve -> fprintf fmt ".."
  | JCurveNoInflex -> fprintf fmt "..."
  | JTension (a,b) -> 
      fprintf fmt "..tension %a and %a .." print_float a print_float b
  | JControls (a,b) -> 
      fprintf fmt "..controls %a and %a .." print_point a print_point b

and print_dir fmt = function
  | NoDir -> ()
  | Vec p -> fprintf fmt "{%a}" print_point p
  | Curl f -> fprintf fmt "{curl %a}" print_float f

and print_knot fmt (d1,p,d2) = 
  fprintf fmt "%a%a%a" print_dir d1 print_point p print_dir d2

and print_dash fmt = function
  | DEvenly -> fprintf fmt "evenly"
  | DWithdots -> fprintf fmt "withdots"
  | DScaled (s, d) -> fprintf fmt "%a scaled %a" print_dash d print_float s
  | DShifted (p, d) -> fprintf fmt "%a shifted %a" print_dash d print_point p
  | DPattern l -> 
      fprintf fmt "dashpattern(";
      List.iter 
	(fun p -> 
	  let p,n = match p with On n -> "on", n | Off n -> "off", n in
	  fprintf fmt "%s %a " p print_num n) 
	l;
      fprintf fmt ")" 

and print_pen fmt = function
  | PenCircle -> fprintf fmt "pencircle"
  | PenSquare -> fprintf fmt "pensquare"
  | PenFromPath p -> fprintf fmt "makepen (%a)" print_path p
  | PenTransformed (p,tr) -> 
      fprintf fmt "%a %a" print_pen p print_transform_list tr

and print_command fmt  = function
  | CDraw (path, color, pen, dashed) ->
      fprintf fmt "@[<hov 2>draw@ %a@,%a@,%a@,%a;@]@\n" print_path path
        (print_option " withcolor " print_color) color
        (print_option " withpen " print_pen) pen
        (print_option " dashed " print_dash) dashed
  | CDrawArrow (path, color, pen, dashed) ->
      fprintf fmt "drawarrow %a%a%a%a;@\n" print_path path
        (print_option " withcolor " print_color) color
        (print_option " withpen " print_pen) pen
        (print_option " dashed " print_dash) dashed
  | CFill (path, color) ->
      fprintf fmt "fill %a%a;@\n" print_path path
        (print_option " withcolor " print_color) color
  | CLabel (pic,pos,p) ->
      fprintf fmt "label%a(%a,@ %a); @\n"
        print_position pos print_picture pic print_point p
  | CDotLabel (pic,pos,p) ->
      fprintf fmt "@[<hov 2>dotlabel%a(%a,@ %a);@]@\n"
        print_position pos print_picture pic print_point p
  | CLoop(from,until,cmd) ->
      for i = from to until - 1 do
	List.iter (print_command fmt) (cmd i);
      done
  | CDrawBox (None, (BCircle (n, _, _, _) | BRect (n, _, _) as b)) ->
      fprintf fmt "%adrawboxed(%a);@\n" declare_box b print_name n
  | CDrawPic p ->
      fprintf fmt "draw %a;@\n" print_picture p
  | CDrawBox (Some _ as c, (BCircle (n, _, _, _) | BRect (n, _, _) as b)) ->
      let fill = CFill (PABoxBPath b, c) in
      fprintf fmt "%a%adrawboxed(%a);@\n" 
	declare_box b print_command fill print_name n
  | CSeq l ->
      List.iter (fun c -> print_command fmt c) l
  | CDeclPath (n, p) ->
      fprintf fmt "path %s ;@\n%s = %a;@\n" n n print_path p
  | CDefPic (pic, cmd) ->
      let savepic = Name.picture () in
      fprintf fmt "picture %s, %s ;@\n" savepic pic;
      fprintf fmt "%s = currentpicture;@\n" savepic;
      fprintf fmt "currentpicture := nullpicture;@\n";
      print_command fmt cmd;
      fprintf fmt "%s = currentpicture;@\n" pic;
      fprintf fmt "currentpicture := %s;@\n" savepic

let print i fmt l =
  Compile.reset ();
  let l = List.map Compile.command l in
  fprintf fmt "@[beginfig(%d)@\n  @[%a@] endfig;@]@." i 
    (fun fmt l -> List.iter (print_command fmt) l)
    l

let generate_mp fn l =
  Misc.write_to_formatted_file fn
    (fun fmt -> 
      Format.fprintf fmt "input boxes;@\n";
      List.iter (fun (i,f) -> print i fmt f) l)

(* batch processing *)

let figures = ref []

let emit i f = figures := (i, f) :: !figures

let dump f = generate_mp f (List.rev !figures)

