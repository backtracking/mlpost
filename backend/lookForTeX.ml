(* look for some piece of TeX *)

open Types
open Hashcons

let num_memoize = Hashtbl.create 50
let point_memoize = Hashtbl.create 50
let metapath_memoize = Hashtbl.create 50
let path_memoize = Hashtbl.create 50
let picture_memoize = Hashtbl.create 50
let command_memoize = Hashtbl.create 50

let memoize f memoize =
  fun acc arg -> 
    try
      Hashtbl.find memoize arg.tag;
      acc
    with
        Not_found -> 
          Hashtbl.add memoize arg.tag ();
          f acc arg.node

let option_compile f acc = function
  | None -> acc
  | Some obj -> f acc obj

let rec num' acc = function
  | F f -> acc
  | NXPart p | NYPart p -> point acc p
  | NAdd(n1,n2) | NSub(n1,n2) | NMult (n1,n2)
  | NDiv (n1,n2) | NMax (n1,n2) | NMin (n1,n2)
  | NGMean (n1,n2) -> num (num acc n1) n2
  | NLength p -> path acc p
and num acc = memoize num' num_memoize acc
and point' acc = function
  | PTPair (f1,f2) -> num (num acc f1) f2
  | PTPointOf (f,p) | PTDirectionOf (f,p)-> path (num acc f) p
  | PTAdd (p1,p2) |PTSub (p1,p2) -> point (point acc p1) p2
  | PTMult (f,p) -> point (num acc f) p
  | PTRotated (_,p) -> point acc p
  | PTPicCorner (pic,_) -> picture acc pic
  | PTTransformed (p,tr) -> point (transform acc tr) p
and point acc = memoize point' point_memoize acc
and knot acc k = 
  match k.Hashcons.node with
    |{ knot_in = d1 ; knot_p = p ; knot_out = d2 } ->
       direction (direction (point acc p) d1) d2
and joint acc j =
  match j.Hashcons.node with
      | JControls (p1,p2) -> point (point acc p1) p2
      | JLine|JCurve|JCurveNoInflex|JTension _ -> acc
and direction acc d = 
  match d.Hashcons.node with
    | Vec p -> point acc p
    | Curl _ | NoDir _ -> acc
and metapath' acc = function
  | MPAConcat (pa,j,p) -> metapath (knot (joint acc j) pa) p
  | MPAAppend (p1,j,p2) -> metapath (metapath (joint acc j) p1) p2
  | MPAKnot k -> knot acc k
  | MPAofPA p -> path acc p
and metapath acc = memoize metapath' metapath_memoize acc
and path' acc = function
  | PAofMPA p -> metapath acc p
  | MPACycle (d,j,p) -> direction (metapath (joint acc j) p) d
  | PATransformed (p,tr) -> path (transform acc tr) p
  | PACutAfter (p1,p2) |PACutBefore (p1,p2) -> path (path acc p1) p2
  | PASub (f1,f2,p) -> num (num (path acc p) f1) f2
  | PABBox p -> picture acc p
  | PABuildCycle p -> List.fold_left path acc p
  | PAUnitSquare | PAQuarterCircle | PAHalfCircle | PAFullCircle -> acc
and path acc = memoize path' path_memoize acc
and picture acc arg =
  try
    Hashtbl.find picture_memoize arg.tag;
    acc
  with
      Not_found -> 
        Hashtbl.add picture_memoize arg.tag ();
        match arg.node with
          | PITransformed (p,tr) -> picture (transform acc tr) p
          | PITex tex -> (arg,tex)::acc
          | PIMake c -> command acc c
          | PIClip (pic,pth) -> picture (path acc pth) pic
and transform acc t =
  match t.Hashcons.node with
    | TRRotated _ -> acc
    | TRScaled f | TRSlanted f | TRXscaled f | TRYscaled f -> num acc f
    | TRShifted p | TRZscaled p | TRRotateAround (p,_)-> point acc p
    | TRReflect (p1,p2) -> point (point acc p1) p2
and dash acc d =
  match d.Hashcons.node with
    | DEvenly | DWithdots -> acc
    | DScaled (_,d) -> dash acc d
    | DShifted (p,d) -> point (dash acc d) p
    | DPattern l -> List.fold_left dash_pattern acc l
and dash_pattern acc o =
  match o.Hashcons.node with
    | On f | Off f -> num acc f
and command' acc = function
  | CDraw (p, _, pe, dsh) | CDrawArrow (p, _, pe, dsh)
      -> path ((option_compile pen) ((option_compile dash) acc dsh) pe) p
  | CDrawPic p ->  picture acc p
  | CFill (p,_) -> path acc p
  | CSeq l -> List.fold_left command acc l
  | CDotLabel (pic,_,pt) | CLabel (pic,_,pt) -> picture (point acc pt) pic
  | CExternalImage _ -> acc
and pen acc p = 
  match p.Hashcons.node with
    | PenCircle | PenSquare -> acc
    | PenFromPath p -> path acc p
    | PenTransformed (p,tr) -> pen (transform acc tr) p

and command acc = memoize command' command_memoize acc

let compile_tex l =
  let tags,texs = List.split l in
  let texs = Gentex.create !Compute.prelude texs in
  List.iter2 (fun tag tex -> Hashtbl.add 
                Compute.picture_memoize tag.tag (Picture_lib.tex tex))
    tags texs

let ct_aux f = fun arg -> compile_tex (f [] arg)
let ct_auxl f = fun argl -> compile_tex (List.fold_left f [] argl)

let commandl = ct_auxl command
let numl = ct_auxl num
let pointl = ct_auxl point
let pathl = ct_auxl path
let metapathl = ct_auxl metapath
let picturel = ct_auxl picture


let command = ct_aux command
let num = ct_aux num
let point = ct_aux point
let path = ct_aux path
let metapath = ct_aux metapath
let picture = ct_aux picture
