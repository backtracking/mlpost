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

open Format
open Misc
open Types
module C = Compiled_types

let externalimage_dimension filename : float * float = 
  let inch = Unix.open_process_in ("identify -format \"%h\\n%w\" "^filename) in
  try let h = float_of_string (input_line inch) in
  let w = float_of_string (input_line inch) in (h,w)
  with End_of_file | Failure "float_of_string" -> 
    invalid_arg "Unknown external image"

let name = pp_print_string

let piccorner fmt p = 
  match corner_reduce p with
  | `Northwest  -> fprintf fmt "ulcorner"
  | `Northeast -> fprintf fmt "urcorner"
  | `Southwest -> fprintf fmt "llcorner"
  | `Southeast -> fprintf fmt "lrcorner"

let position fmt p = 
  match pos_reduce p with
  | `Center  -> fprintf fmt ""
  | `West   -> fprintf fmt ".lft"
  | `East  -> fprintf fmt ".rt"
  | `North    -> fprintf fmt ".top"
  | `South    -> fprintf fmt ".bot"
  | `Northwest  -> fprintf fmt ".ulft"
  | `Northeast -> fprintf fmt ".urt"
  | `Southwest -> fprintf fmt ".llft"
  | `Southeast -> fprintf fmt ".lrt"

let rec num fmt = function
  | C.F f ->
      if f = infinity then fprintf fmt "infinity"
      else if f > 4095. then fprintf fmt "%g" 4095.
      else if abs_float f < 0.0001 then fprintf fmt "0"
      else fprintf fmt "%g" f
  | C.NXPart p -> fprintf fmt "xpart %a" point p
  | C.NYPart p -> fprintf fmt "ypart %a" point p
  | C.NAdd (f1, f2) -> fprintf fmt "(%a +@ %a@,)" num f1 num f2
  | C.NSub (f1, f2) -> fprintf fmt "(%a -@ %a@,)" num f1 num f2
  | C.NMult (f1, f2) -> fprintf fmt "(%a *@ %a@,)" num f1 num f2
  | C.NDiv (f1, f2) -> fprintf fmt "(%a /@ %a@,)" num f1 num f2
  | C.NMax (f1, f2) -> fprintf fmt "max(@ %a,@ %a@,)" num f1 num f2
  | C.NMin (f1, f2) -> fprintf fmt "min(@ %a,@ %a@,)" num f1 num f2
  | C.NGMean (f1, f2) -> fprintf fmt "(%a@ ++@ %a@,)" num f1 num f2
  | C.NName n -> pp_print_string fmt n
  | C.NLength p -> fprintf fmt "length (%a@,)" path p
  | C.NIfnullthenelse (n,n1,n2) -> fprintf fmt "(if (%a = 0): %a else: %a fi)" num n num n1 num n2

and float fmt f = num fmt (C.F f)

and scolor fmt = function
  | RGB (r,g,b) -> fprintf fmt "(%a, %a , %a@,)" float r float g float b
  | CMYK (c,m,y,k) ->
      fprintf fmt "(%a, %a, %a, %a@,)" float c float m float y float k
  | Gray f -> fprintf fmt "%a * white" float f

and color fmt = function
  | OPAQUE c -> scolor fmt c
  | TRANSPARENT (f,c) -> fprintf fmt "transparent (1,%a,%a@,)" float f scolor c
      (* 1 is the "normal" mode *)

and point fmt = function
  | C.PTPair (m,n) -> fprintf fmt "(%a,@ %a@,)" num m num n
  | C.PTPicCorner (pic, d) -> fprintf fmt "(%a@ %a@,)" piccorner d picture pic
  | C.PTAdd (p1, p2) -> fprintf fmt "(%a +@ %a@,)" point p1 point p2
  | C.PTSub (p1, p2) -> fprintf fmt "(%a -@ %a@,)" point p1 point p2
  | C.PTMult (f, p) -> fprintf fmt "(%a *@ %a@,)" num f point p
  | C.PTRotated (f, p) ->  fprintf fmt "(%a rotated@ %a@,)" point p float f
  | C.PTPointOf (f, p) -> fprintf fmt "(point %a@ of (%a))" num f path p
  | C.PTDirectionOf (f, p) ->
      fprintf fmt "(direction %a@ of (%a))" num f path p
  | C.PTTransformed (p,tr) -> fprintf fmt "((%a)@ %a@,)" point p transform tr
  | C.PTName pn -> pp_print_string fmt pn

and transform fmt = function
  | C.TRScaled a -> fprintf fmt "scaled %a@," num a
  | C.TRShifted a -> fprintf fmt "shifted %a@," point a
  | C.TRRotated a -> fprintf fmt "rotated %a@," float a
  | C.TRSlanted a -> fprintf fmt "slanted %a@," num a
  | C.TRXscaled a -> fprintf fmt "xscaled %a@," num a
  | C.TRYscaled a -> fprintf fmt "yscaled %a@," num a
  | C.TRZscaled a -> fprintf fmt "zscaled %a@," point a
  | C.TRReflect (p1,p2) -> 
      fprintf fmt "reflectedabout (%a,@ %a)@," point p1 point p2
  | C.TRRotateAround (p,f) -> fprintf fmt "rotatedaround(%a,@ %a)@," point p float f

and picture fmt = function
  | C.PITex s -> fprintf fmt "btex %s etex" s
  | C.PITransformed (p, tr) -> fprintf fmt "((%a) %a@,)" picture p transform tr
  | C.PIName n -> pp_print_string fmt n

and path fmt = function
    C.PAScope p -> fprintf fmt "(%a@,)" path p
  | C.PAFullCircle -> fprintf fmt "fullcircle"
  | C.PAHalfCircle -> fprintf fmt "halfcircle"
  | C.PAQuarterCircle -> fprintf fmt "quartercircle"
  | C.PAUnitSquare -> fprintf fmt "unitsquare"
  | C.PATransformed (p,tr) -> fprintf fmt "((%a) %a@,)" path p transform tr
  | C.PAAppend (p1,j,p2) -> fprintf fmt "%a %a@ %a" path p1 joint j path p2
  | C.PACycle (d,j,p) -> fprintf fmt "%a %a %acycle" path p joint j direction d
  | C.PAConcat (k,j,p) -> fprintf fmt "%a %a@ %a" path p joint j knot k
  | C.PAKnot k -> knot fmt k
  | C.PACutAfter (p1, p2) -> fprintf fmt "%a cutafter (%a)@ " path p2 path p1
  | C.PACutBefore (p1, p2) -> fprintf fmt "%a cutbefore (%a)@ " path p2 path p1
  | C.PABuildCycle l ->
      fprintf fmt "buildcycle(%a@,)" (Misc.print_list comma path) l
  | C.PASub (f1, f2, p) ->
      fprintf fmt "subpath(%a,%a) of %a" num f1 num f2 name p
  | C.PABBox p -> fprintf fmt "bbox %a" picture p
  | C.PAName n -> pp_print_string fmt n

and joint fmt = function
  | C.JLine -> fprintf fmt "--"
  | C.JCurve -> fprintf fmt ".."
  | C.JCurveNoInflex -> fprintf fmt "..."
  | C.JTension (a,b) -> fprintf fmt "..tension %a and %a .." float a float b
  | C.JControls (a,b) -> fprintf fmt "..controls %a and %a .." point a point b

and direction fmt = function
  | C.NoDir -> ()
  | C.Vec p -> fprintf fmt "{%a}" point p
      (* Why there is not the same thing than in Num?*)
  | C.Curl f -> fprintf fmt "{curl %a}" float f

and knot fmt (d1,p,d2) = fprintf fmt "%a%a%a" direction d1 point p direction d2

and dash fmt = function
  | C.DEvenly -> fprintf fmt "evenly"
  | C.DWithdots -> fprintf fmt "withdots"
  | C.DScaled (s, d) -> fprintf fmt "%a scaled %a" dash d float s
  | C.DShifted (p, d) -> fprintf fmt "%a shifted %a" dash d point p
  | C.DPattern l -> 
      fprintf fmt "dashpattern(";
      List.iter 
	(fun p -> 
	  let p,n = match p with C.On n -> "on", n | C.Off n -> "off", n in
	    fprintf fmt "%s %a " p num n) 
	l;
      fprintf fmt ")" 

and pen fmt = function
  | C.PenCircle -> fprintf fmt "pencircle"
  | C.PenSquare -> fprintf fmt "pensquare"
  | C.PenFromPath p -> fprintf fmt "makepen (%a@,)" path p
  | C.PenTransformed (p,tr) -> fprintf fmt "%a %a" pen p transform tr

and command fmt  = function
  | C.CDraw (pa, c, pe, dashed) ->
      fprintf fmt "@[<hov 2>draw@ %a@,%a@,%a@,%a;@]@\n" path pa
        (print_option " withcolor " color) c
        (print_option " withpen " pen) pe
        (print_option " dashed " dash) dashed
  | C.CDrawArrow (pa, c, pe, dashed) ->
      fprintf fmt "drawarrow %a%a%a%a;@\n" path pa
        (print_option " withcolor " color) c
        (print_option " withpen " pen) pe
        (print_option " dashed " dash) dashed
  | C.CFill (pa, c) ->
      fprintf fmt "fill %a%a;@\n" path pa (print_option " withcolor " color) c
  | C.CLabel (pic,pos,p) ->
      fprintf fmt "label%a(%a,@ %a); @\n" position pos picture pic point p
  | C.CDotLabel (pic,pos,p) ->
      fprintf fmt "@[<hov 2>dotlabel%a(%a,@ %a);@]@\n"
        position pos picture pic point p
  | C.CDrawPic p -> fprintf fmt "draw %a;@\n" picture p
  | C.CSeq l -> List.iter (fun c -> command fmt c) l
  | C.CDeclPath (n, p) -> fprintf fmt "path %s ;@\n%s = %a;@\n" n n path p
  | C.CDeclPoint (n, p) -> fprintf fmt "pair %s ;@\n%s = %a;@\n" n n point p
  | C.CDeclNum (n, nm) -> fprintf fmt "numeric %s ;@\n%s = %a;@\n" n n num nm
  | C.CSimplePic (pn1, pexpr) ->
      fprintf fmt "picture %s;@\n" pn1;
      fprintf fmt "%s := %a;@\n" pn1 picture pexpr;
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
      command fmt cmd;
      fprintf fmt "%s = currentpicture;@\n" pic;
      fprintf fmt "currentpicture := %s;@\n" savepic
  | C.CClip (pic,pth) -> fprintf fmt "clip %s to %a;@\n" pic path pth
  | C.CExternalImage (filename, spec) ->
      match spec with
        | `Exact (h,w) -> 
            fprintf fmt "externalfigure \"%s\" xyscaled (%a,%a);@\n" 
              filename num w num h
        | ((`None as spec) | (`Height _ as spec)| 
          (`Width _ as spec)| (`Inside _ as spec)) -> 
            let fh,fw = externalimage_dimension filename in
            let printext h w = 
              fprintf fmt "externalfigure \"%s\" xyscaled (%a,%a);@\n" 
                filename num w num h in
              match spec with
                | `None -> printext (C.F fh) (C.F fw)
                | `Height h -> printext h (C.NMult (C.F (fw/.fh),h))
                | `Width w -> printext (C.NMult (C.F (fh/.fw),w)) w
                | `Inside (h,w) -> 
                    let w = C.NMin (C.NMult (h,C.F (fw/.fh)),w) in
                    printext (C.NMult (C.F (fh/.fw),w)) w

