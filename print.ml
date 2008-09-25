(* print an mlpost AST for debugging *)
open Types
open Format

let option_print pr fmt = function
  | None -> ()
  | Some x -> pr fmt x

let piccorner fmt = function
  | UL -> fprintf fmt "ulcorner"
  | LL -> fprintf fmt "llcorner"
  | UR -> fprintf fmt "urcorner"
  | LR -> fprintf fmt "lrcorner"

let position fmt = function
  | Pcenter  -> fprintf fmt ""
  | Pleft   -> fprintf fmt ".lft"
  | Pright  -> fprintf fmt ".rt"
  | Ptop    -> fprintf fmt ".top"
  | Pbot    -> fprintf fmt ".bot"
  | Pupleft  -> fprintf fmt ".ulft"
  | Pupright -> fprintf fmt ".urt"
  | Plowleft -> fprintf fmt ".llft"
  | Plowright -> fprintf fmt ".lrt"

let rec num fmt n = 
  match n.Hashcons.node with
  | F f -> fprintf fmt "%f" f
  | NXPart p -> fprintf fmt "xpart(%a)" point p
  | NYPart p -> fprintf fmt "ypart(%a)" point p
  | NAdd(n1,n2) -> fprintf fmt "(%a + %a)" num n1 num n2
  | NSub(n1,n2) -> fprintf fmt "(%a - %a)" num n1 num n2
  | NMult(n1,n2) -> fprintf fmt "(%a * %a)" num n1 num n2
  | NDiv(n1,n2) -> fprintf fmt "(%a / %a)" num n1 num n2
  | NMax(n1,n2) -> fprintf fmt "max(%a,%a)" num n1 num n2
  | _ -> assert false

and point fmt p = 
  match p.Hashcons.node with
  | PTPair (n1,n2) -> fprintf fmt "(%a,%a)" num n1 num n2
  | PTPicCorner (p,pc) -> fprintf fmt "%a(%a)" piccorner pc picture p
  | PTAdd (p1,p2) -> fprintf fmt "(%a + %a)" point p1 point p2
  | PTSub (p1,p2) -> fprintf fmt "(%a - %a)" point p1 point p2
  | PTMult (n,p) -> fprintf fmt "(%a * %a)" num n point p
  | _ -> assert false
and picture fmt p = 
  match p.Hashcons.node with
  | PITex s -> fprintf fmt "tex(%s)" s
  | PIMake _ -> ()
  | PITransform (tr,p) -> 
      fprintf fmt "%a transformed %a" picture p transform_list tr
  | PIClip _ -> ()

and transform fmt t = 
  match t.Hashcons.node with
    | TRShifted p -> fprintf fmt "shifted %a" point p
    | _ -> assert false

and transform_list fmt l =
  Misc.print_list Misc.space transform fmt l

and knot fmt k =
  match k.Hashcons.node with
  | { knot_in = d1 ; knot_p = p ; knot_out = d2 } ->
      fprintf fmt "%a%a%a" direction d1 point p direction d2

and direction fmt d = 
  match d.Hashcons.node with
  | Vec p -> fprintf fmt "{%a}" point p
  | Curl f -> fprintf fmt "{curl %f}" f
  | NoDir  -> ()

and path fmt p = 
  match p.Hashcons.node with
  | PACycle (d,j,p) -> 
      fprintf fmt "%a%acycle" path p joint j
  | PAConcat (k,j,p) -> 
      fprintf fmt "%a%a%a" path p joint j knot k
  | PATransformed (p,tr) -> assert false
  | PACutAfter (p1,p2) -> assert false
  | PACutBefore (p1,p2) -> assert false
  | PAAppend (p1,j,p2) -> 
      fprintf fmt "%a%a%a" path p1 joint j path p2
  | PABuildCycle pl -> assert false
  | PASub (f1, f2, p) -> assert false
  | PABBox p -> assert false
  | PAKnot k -> knot fmt k
  | PAUnitSquare -> fprintf fmt "unitsquare"
  | PAQuarterCircle -> fprintf fmt "quartercircle"
  | PAHalfCircle -> fprintf fmt "halfcircle"
  | PAFullCircle -> fprintf fmt "fullcircle"
and joint fmt j = 
  match j.Hashcons.node with
  | JLine -> fprintf fmt "--"
  | JCurve -> fprintf fmt ".."
  | JCurveNoInflex -> fprintf fmt "..."
  | JTension (a,b) -> fprintf fmt "..tension(%f,%f).." a b
  | JControls (p1,p2) -> fprintf fmt "..%a..%a.." point p1 point p2

let rec command fmt c = 
  match c.Hashcons.node with 
  | CDraw (p,c,pe,d) ->
      fprintf fmt "draw (%a,%a,%a,%a);" 
      path p option_color c option_pen pe option_dash d
  | CDrawArrow _ -> assert false
  | CDrawPic p -> fprintf fmt "draw_pic (%a);@ " picture p
  | CFill _ -> assert false
  | CLabel _ -> assert false
  | CDotLabel (pic,pos,pt) ->
      fprintf fmt "dotlabel%a(%a,%a)" position pos picture pic point pt
  | CSeq l -> Misc.print_list Misc.space command fmt l
and color fmt = function
  | RGB (r,g,b) -> 
      fprintf fmt "(%f, %f , %f)" r g b
  | CMYK (c,m,y,k) ->
      fprintf fmt "(%f, %f, %f, %f)" c m y k
  | Gray f -> fprintf fmt "%f * white" f
and pen fmt x =
  match x.Hashcons.node with
  | PenCircle -> assert false
  | PenSquare -> assert false
  | PenFromPath p -> assert false
  | PenTransformed (p,trl) -> assert false

and dash fmt x = 
  match x.Hashcons.node with
  | DEvenly -> fprintf fmt "evenly"
  | DWithdots -> assert false
  | DScaled (f,d) -> fprintf fmt "%a scaled %f" dash d f
  | DShifted (p,d) -> assert false
  | DPattern l -> assert false
and option_pen fmt = option_print pen fmt
and option_color fmt = option_print color fmt
and option_dash fmt = option_print dash fmt

