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

(* print an mlpost AST for debugging *)
open Types
open Format

let option_print pr fmt = function
  | None -> ()
  | Some x -> pr fmt x

let position fmt = function
  | `Center  -> fprintf fmt ""
  | `Left   -> fprintf fmt ".lft"
  | `Right  -> fprintf fmt ".rt"
  | `Top    -> fprintf fmt ".top"
  | `Bot    -> fprintf fmt ".bot"
  | `Upleft  -> fprintf fmt ".ulft"
  | `Upright -> fprintf fmt ".urt"
  | `Lowleft -> fprintf fmt ".llft"
  | `Lowright -> fprintf fmt ".lrt"

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
  | PTPicCorner (p,pc) -> fprintf fmt "%a(%a)" position pc picture p
  | PTAdd (p1,p2) -> fprintf fmt "(%a + %a)" point p1 point p2
  | PTSub (p1,p2) -> fprintf fmt "(%a - %a)" point p1 point p2
  | PTMult (n,p) -> fprintf fmt "(%a * %a)" num n point p
  | _ -> assert false
and picture fmt p = 
  match p.Hashcons.node with
  | PITex s -> fprintf fmt "tex(%s)" s
  | PIMake _ -> ()
  | PITransformed (p,tr) -> 
      fprintf fmt "%a transformed %a" picture p transform tr
  | PIClip _ -> ()

and transform fmt t = 
  match t.Hashcons.node with
    | TRShifted p -> fprintf fmt "shifted %a" point p
    | _ -> assert false

and knot fmt k =
  match k.Hashcons.node with
  | { knot_in = d1 ; knot_p = p ; knot_out = d2 } ->
      fprintf fmt "%a%a%a" direction d1 point p direction d2

and direction fmt d = 
  match d.Hashcons.node with
  | Vec p -> fprintf fmt "{%a}" point p
  | Curl f -> fprintf fmt "{curl %f}" f
  | NoDir  -> ()

and metapath fmt p =
  match p.Hashcons.node with
  | MPAConcat (k,j,p) -> 
      fprintf fmt "%a%a%a" metapath p joint j knot k
  | MPAAppend (p1,j,p2) -> 
      fprintf fmt "%a%a%a" metapath p1 joint j metapath p2
  | MPAKnot k -> knot fmt k
  | MPAofPA p -> fprintf fmt "(%a)" path p
and path fmt p = 
  match p.Hashcons.node with
  | PAofMPA p -> fprintf fmt "(%a)" metapath p
  | MPACycle (d,j,p) -> 
      fprintf fmt "%a%acycle" metapath p joint j
  | PATransformed (p,tr) -> assert false
  | PACutAfter (p1,p2) -> assert false
  | PACutBefore (p1,p2) -> assert false
  | PABuildCycle pl -> assert false
  | PASub (f1, f2, p) -> assert false
  | PABBox p -> assert false
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
  | CExternalImage (f,spec) -> fprintf fmt "externalimage %s@ " f
and color fmt (c:Types.color) = 
  let mode,color = match c with
    | OPAQUE c -> (None, c)
    | TRANSPARENT (f,c) -> (Some f, c) in
  let pmode fmt = function
    | None -> fprintf fmt "O"
    | Some f -> fprintf fmt "%f" f in
  match color with
  | RGB (r,g,b) -> 
      fprintf fmt "(%f, %f , %f, %a)" r g b pmode mode
  | CMYK (c,m,y,k) ->
      fprintf fmt "(%f, %f, %f, %f, %a)" c m y k pmode mode
  | Gray f -> fprintf fmt "(%f * white, %a)" f pmode mode
and pen fmt x =
  match x.Hashcons.node with
  | PenCircle -> assert false
  | PenSquare -> assert false
  | PenFromPath p -> assert false
  | PenTransformed (p,trl) -> fprintf fmt "(%a,%a)" pen p transform trl

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

