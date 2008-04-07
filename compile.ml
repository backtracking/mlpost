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

open Types
module C = Compiled_types

let nop = C.CSeq []
let (++) c1 c2 =
  match c1,c2 with
    | C.CSeq [], _ -> c2
    | _, C.CSeq [] -> c1
    | _, _ -> C.CSeq [c1 ; c2]


module HPic = Hashtbl.Make(struct 
  type t = picture let equal = (==) let hash = Hashtbl.hash 
end)
let known_pictures = HPic.create 17

module HPath = Hashtbl.Make(struct 
  type t = path let equal = (==) let hash = Hashtbl.hash 
end)
let known_paths = HPath.create 17

let option_compile f = function
  | None -> None, nop
  | Some obj -> 
      let obj, c = f obj in
        Some obj, c

let rec point = function
  | PTPair (f1,f2) -> C.PTPair (f1,f2), nop
  | PTPointOf (f,p) -> 
      let p, code = path p in
        C.PTPointOf (f,p) , code
  | PTAdd (p1,p2) -> 
      let p1,c1 = point p1 in
      let p2,c2 = point p2 in
        C.PTAdd (p1,p2),  c1 ++ c2
  | PTSub (p1,p2) ->
      let p1,c1 = point p1 in
      let p2,c2 = point p2 in
        C.PTSub (p1,p2),  c1 ++ c2
  | PTMult (f,p) ->
      let p1,c1 = point p in
        C.PTMult (f,p1), c1
  | PTRotated (f,p) ->
      let p1,c1 = point p in
        C.PTRotated (f,p1), c1
  | PTPicCorner (pic, corner) ->
      let pic, code = picture pic in
        C.PTPicCorner (pic, corner) , code
  | PTBoxCorner (n, corner) ->
      C.PTBoxCorner (n, corner), nop
  | PTTransformed (p,tr) ->
      let p, c1 = point p in
      let tr, c2 = transform_list tr in
        C.PTTransformed (p,tr),  c1 ++ c2

and path = function
  | PASub (f1, f2, p) ->
      begin 
	try 
	  let p = HPath.find known_paths p in
	  C.PASub (f1,f2, p), nop
	with Not_found ->
          let n = Name.path () in
          let () = HPath.add known_paths p n in
          let p, code = path p in
            C.PASub (f1, f2, n), code ++ C.CDeclPath (n, p)
      end
  | PABBox p ->
      let p, code = picture p in
        C.PABBox p, code
  | PAConcat (k,j,p) ->
      let p, c1 = path p in
      let k, c2 = knot k in
      let j, c3 = joint j in
        C.PAConcat (k,j,p), c1 ++ c2 ++ c3
  | PACycle (d,j,p) ->
      let d, c1 = direction d in
      let j, c2 = joint j in
      let p, c3 = path p in
        C.PACycle (d,j,p), c1 ++ c2 ++ c3
  | PATransformed (p,tr) ->
      let p, c1 = path p in
      let tr, c2 = transform_list tr in
        C.PATransformed (p,tr), c1 ++ c2
  | PAAppend (p1,j,p2) ->
      let p1, c1 = path p1 in
      let j, c2 = joint j in
      let p2, c3 = path p2 in
        C.PAAppend (p1,j,p2), c1 ++ c2 ++ c3
  | PACutAfter (p1,p2) ->
      let p1, c1 = path p1 in
      let p2, c2 = path p2 in
        C.PACutAfter (p1,p2), c1 ++ c2
  | PACutBefore (p1,p2) ->
      let p1, c1 = path p1 in
      let p2, c2 = path p2 in
        C.PACutBefore (p1,p2), c1 ++ c2
  | PABuildCycle pl ->
      let npl = List.map path pl in
        C.PABuildCycle (List.map fst npl), C.CSeq (List.map snd npl)
  | PABoxBPath b ->
      let b, code = box b in
        C.PABoxBPath b, code
  | PAUnitSquare -> C.PAUnitSquare, nop
  | PAQuarterCircle -> C.PAQuarterCircle, nop
  | PAHalfCircle -> C.PAHalfCircle, nop
  | PAFullCircle -> C.PAFullCircle, nop
  | PAName n -> C.PAName n, nop
  | PAKnot k ->
      let k, code = knot k in
        C.PAKnot k, code

and knot (d1,p,d2) =
  let d1, c1 = direction d1 in
  let p, c2 = point p in
  let d2, c3 = direction d2 in
    (d1,p,d2), c1 ++ c2 ++ c3

and joint = function
  | JLine -> C.JLine, nop
  | JCurve -> C.JCurve, nop
  | JCurveNoInflex -> C.JCurveNoInflex, nop
  | JTension (a,b) -> C.JTension (a,b), nop
  | JControls (p1,p2) ->
      let p1,c1 = point p1 in
      let p2,c2 = point p2 in
        C.JControls (p1,p2), c1 ++ c2

and direction = function
  | Vec p -> 
      let p, code = point p in
        C.Vec p, code
  | Curl f -> C.Curl f, nop
  | NoDir  -> C.NoDir, nop

and transform = function
  | TRRotated f -> C.TRRotated f, nop
  | TRScaled f -> C.TRScaled f, nop
  | TRSlanted f -> C.TRSlanted f, nop
  | TRXscaled f -> C.TRXscaled f, nop
  | TRYscaled f -> C.TRYscaled f, nop
  | TRShifted p -> 
      let p, code = point p in
        C.TRShifted p, code
  | TRZscaled p -> 
      let p, code = point p in
        C.TRZscaled p, code
  | TRReflect (p1,p2) ->
      let p1, c1 = point p1 in
      let p2, c2 = point p2 in
        C.TRReflect (p1,p2), c1 ++ c2
  | TRRotateAround (p,f) ->
      let p, code = point p in
        C.TRRotateAround (p,f), code
and transform_list l =
  let l1,l2 = List.fold_right
                (fun tr (trl, cl) -> 
                   let tr,c =  transform tr in
                     tr::trl, c::cl ) l ([],[]) in
    l1, C.CSeq l2

and picture = function
  | PIMake c as p ->
      begin 
	try 
          let pic = HPic.find known_pictures p in
	  C.PIName pic, nop
	with Not_found ->
	  let pic = Name.picture () in
	  HPic.add known_pictures p pic;
	  C.PIName pic, C.CDefPic (pic, command c)
      end
  | PITransform (tr, p) ->
      let tr, c1 = transform_list tr in
      let p, c2 = picture p in
      C.PITransform (tr, p), c1 ++ c2
  | PIName n -> C.PIName n, nop 
  | PITex s -> C.PITex s, nop 

and box = function
  | BCircle (n, p, pic, s) ->
      let p, c1 = point p in
      let pic, c2 = picture pic in
      C.BCircle (n, p, pic, s), c1 ++ c2
  | BRect (n, p, pic) ->
      let p, c1 = point p in
      let pic, c2 = picture pic in
      C.BRect (n, p, pic), c1 ++ c2

and pen = function
  | PenCircle -> C.PenCircle, nop
  | PenSquare -> C.PenCircle, nop
  | PenFromPath p -> 
      let p, code = path p in
        C.PenFromPath p, code
  | PenTransformed (p, tr) ->
      let p, c1 = pen p in
      let tr, c2 = transform_list tr in
        C.PenTransformed (p,tr), c1 ++ c2


and dash = function
  | DEvenly -> C.DEvenly, nop
  | DWithdots -> C.DWithdots, nop
  | DScaled (f, d) -> 
      let d,c = dash d in
        C.DScaled (f,d) , c
  | DShifted (p,d) ->
      let p, c1 = point p in
      let d, c2 = dash d in
        C.DShifted (p,d), c1 ++ c2
  | DPattern l -> C.DPattern l, nop

and command = function
  | CDraw (p, color, pe, dsh) ->
      let p, c1 = path p in
      let pe, c2 = (option_compile pen) pe in
      let dsh, c3 = (option_compile dash) dsh in
      C.CSeq [c1; c2; c3; C.CDraw (p, color, pe, dsh)]
  | CDrawArrow (p, color, pe, dsh) ->
      let p, c1 = path p in
      let pe, c2 = (option_compile pen) pe in
      let dsh, c3 = (option_compile dash) dsh in
      C.CSeq [c1; c2; c3; C.CDrawArrow (p, color, pe, dsh)]
  | CDrawPic p ->
      let p, code = picture p in
      C.CSeq [code; C.CDrawPic p]
  | CDrawBox (c, bx, b) ->
      let b, code = box b in
      C.CSeq [code; C.CDrawBox (c, bx, b)]
  | CFill (p, c) ->
      let p, code = path p in
      C.CSeq [code; C.CFill (p, c)]
  | CSeq l ->
      C.CSeq (List.map command l)
  | CLoop (i, j, f) ->
      C.CLoop (i, j, fun k -> command (f k))
  | CDotLabel (pic, pos, pt) -> 
      let pic, c1 = picture pic in
      let pt, c2 = point pt in
        c1 ++ c2 ++ C.CDotLabel (pic,pos,pt)
  | CLabel (pic, pos ,pt) -> 
      let pic, c1 = picture pic in
      let pt, c2 = point pt in
      c1 ++ c2 ++ C.CLabel (pic,pos,pt)

let reset () = 
  HPath.clear known_paths;
  HPic.clear known_pictures
