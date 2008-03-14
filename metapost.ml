
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

and print_float = print_num

and print_point fmt = function
  | PTPair (m,n) -> fprintf fmt "(%a,%a)" print_num m print_num n
  | PTBoxCorner (n, d) -> fprintf fmt "%a.%a" print_name n print_corner d
  | PTAdd (p1, p2) -> fprintf fmt "(%a + %a)" print p1 print p2
  | PTSub (p1, p2) -> fprintf fmt "(%a - %a)" print p1 print p2
  | PTMult (f, p) -> fprintf fmt "(%a * %a)" print_float f print p
  | PTRotated (f, p) ->  
      fprintf fmt "(%a rotated %a)" print p print_float f

and print_transform fmt = function
  | TRScaled a -> F.fprintf fmt "scaled %a@ " print_num a
  | TRShifted a -> F.fprintf fmt "shifted %a@ " print_point a
  | TRRotated a -> F.fprintf fmt "rotated %a@ " print_num_float a
  | TRSlanted a -> F.fprintf fmt "slanted %a@ " print_num a
  | TRXscaled a -> F.fprintf fmt "xscaled %a@ " print_num a
  | TRYscaled a -> F.fprintf fmt "yscaled %a@ " print_num a
  | TRZscaled a -> F.fprintf fmt "zscaled %a@ " print_point a
  | TRReflect (p1,p2) -> 
      F.fprintf fmt "reflectedabout (%a,%a)@ " 
        print_point p1 print_point p2
  | TRRotateAround (p,f) ->
      F.fprintf fmt "rotatedaround(%a,%a)@ "
        print_point p print_num_float f

and print_transform_list fmt l =
  Misc.print_list (fun fmt () -> F.fprintf fmt " ") print_transform fmt l

and print_picture fmt = function
  | PITex s -> fprintf fmt "btex %s etex" s
