
open Format
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

and print_float fmt f = print_num fmt f

and print_point fmt = function
  | PTPair (m,n) -> fprintf fmt "(%a,%a)" print_num m print_num n
  | PTBoxCorner (n, d) -> fprintf fmt "%a.%a" print_name n print_corner d
  | PTAdd (p1, p2) -> fprintf fmt "(%a + %a)" print_point p1 print_point p2
  | PTSub (p1, p2) -> fprintf fmt "(%a - %a)" print_point p1 print_point p2
  | PTMult (f, p) -> fprintf fmt "(%a * %a)" print_float f print_point p
  | PTRotated (f, p) ->  
      fprintf fmt "(%a rotated %a)" print_point p print_float f
  | PTPointOf (f, p) ->
      fprintf fmt "(point %a of %a)" print_float f print_path p

and print_transform fmt = function
  | TRScaled a -> fprintf fmt "scaled %a@ " print_num a
  | TRShifted a -> fprintf fmt "shifted %a@ " print_point a
  | TRRotated a -> fprintf fmt "rotated %a@ " print_float a
  | TRSlanted a -> fprintf fmt "slanted %a@ " print_num a
  | TRXscaled a -> fprintf fmt "xscaled %a@ " print_num a
  | TRYscaled a -> fprintf fmt "yscaled %a@ " print_num a
  | TRZscaled a -> fprintf fmt "zscaled %a@ " print_point a
  | TRReflect (p1,p2) -> 
      fprintf fmt "reflectedabout (%a,%a)@ " 
        print_point p1 print_point p2
  | TRRotateAround (p,f) ->
      fprintf fmt "rotatedaround(%a,%a)@ "
        print_point p print_float f

and print_transform_list fmt l =
  Misc.print_list (fun fmt () -> fprintf fmt " ") print_transform fmt l

and print_picture fmt = function
  | PITex s -> fprintf fmt "btex %s etex" s

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
      fprintf fmt "%a %a %a" print_path p print_joint j print_knot k
  | PAKnot k -> print_knot fmt k
  | PABoxBPath (BCircle (n, _, _, _) | BRect (n, _, _)) ->
      fprintf fmt "bpath.%a" print_name n
  | PACutAfter (p1, p2) -> 
      fprintf fmt "%a cutafter %a@ " print_path p2 print_path p1
  | PACutBefore (p1, p2) -> 
      fprintf fmt "%a cutbefore %a@ " print_path p2 print_path p1
  | PABuildCycle l ->
      fprintf fmt "buildcycle(%a)" 
        (Misc.print_list (fun fmt () -> fprintf fmt ",") print_path) l
  | PASub (f1, f2, p) ->
      fprintf fmt "subpath(%a,%a) of %a" 
	print_float f1 print_float f2 print_path p

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
