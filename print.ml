(* print an mlpost AST for debugging *)
open Types
open Format

let piccorner fmt = function
  | UL -> fprintf fmt "ulcorner"
  | LL -> fprintf fmt "llcorner"
  | UR -> fprintf fmt "urcorner"
  | LR -> fprintf fmt "lrcorner"

let rec num fmt = function
  | F f -> fprintf fmt "%f" f
  | NXPart p -> fprintf fmt "xpart(%a)" point p
  | NYPart p -> fprintf fmt "ypart(%a)" point p
  | NAdd (n1,n2) -> fprintf fmt "(%a + %a)" num n1 num n2
  | NMinus (n1,n2) -> fprintf fmt "(%a - %a)" num n1 num n2
  | NMult (n1,n2) -> fprintf fmt "(%a * %a)" num n1 num n2
  | NDiv (n1,n2) -> fprintf fmt "(%a / %a)" num n1 num n2
  | NMax (n1,n2) -> fprintf fmt "max(%a,%a)" num n1 num n2
  | _ -> assert false

and point fmt = function
  | PTPair (n1,n2) -> fprintf fmt "(%a,%a)" num n1 num n2
  | PTPicCorner (p,pc) -> fprintf fmt "%a(%a)" piccorner pc picture p
  | PTAdd (p1,p2) -> fprintf fmt "(%a + %a)" point p1 point p2
  | PTSub (p1,p2) -> fprintf fmt "(%a - %a)" point p1 point p2
  | PTMult (n,p) -> fprintf fmt "(%a * %a)" num n point p
  | _ -> assert false
and picture fmt = function
  | PITex s -> fprintf fmt "tex(%s)" s
  | PIMake _ -> ()
  | PITransform (tr,p) -> 
      fprintf fmt "%a transformed %a" picture p transform_list tr
  | PIClip _ -> ()

and transform fmt = function
  | TRShifted p -> fprintf fmt "shifted %a" point p
  | _ -> assert false

and transform_list fmt l =
  Misc.print_list Misc.space transform fmt l


let rec command fmt = function
(*
  | CDraw (p,c,p,d) ->
      fprintf fmt "draw (%a);" path p
*)
  | CDraw _ -> ()
  | CDrawArrow _ -> ()
  | CDrawPic p -> fprintf fmt "draw_pic (%a);@ " picture p
  | CFill _ -> ()
  | CLabel _ -> ()
  | CDotLabel _ -> ()
  | CLoop _ -> ()
  | CSeq l -> Misc.print_list Misc.space command fmt l

