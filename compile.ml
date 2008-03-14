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

let nop = CSeq []

let known_pictures = Hashtbl.create 17
let known_paths = Hashtbl.create 17

let rec path = function
  | PASub (f1, f2, p) ->
      begin 
	try 
	  let p = Hashtbl.find known_paths p in
	  PASub (f1,f2, PAName p), nop
	with Not_found ->
          let n = Name.path () in
          let () = Hashtbl.add known_paths p n in
          let p, code = path p in
            PASub (f1, f2, PAName n), CSeq [code; CDeclPath (n, p)]
      end
  | PABBox p ->
      let p, code = picture p in
        PABBox p, code
  | PAConcat (k,j,p) ->
      let p, code = path p in
        PAConcat (k,j,p), code
  | PACycle (d,j,p) ->
      let p, code = path p in
        PACycle (d,j,p), code
  | PATransformed (p,tr) ->
      let p, code = path p in
        PATransformed (p,tr), code
  | PAAppend (p1,j,p2) ->
      let p1, c1 = path p1 in
      let p2, c2 = path p2 in
        PAAppend (p1,j,p2), CSeq [c1; c2]
  | PACutAfter (p1,p2) ->
      let p1, c1 = path p1 in
      let p2, c2 = path p2 in
        PACutAfter (p1,p2), CSeq [c1; c2]
  | PACutBefore (p1,p2) ->
      let p1, c1 = path p1 in
      let p2, c2 = path p2 in
        PACutBefore (p1,p2), CSeq [c1; c2]
  | PABuildCycle pl ->
      let npl = List.map path pl in
        PABuildCycle (List.map fst npl), CSeq (List.map snd npl)
  | PABoxBPath b ->
      let b, code = box b in
        PABoxBPath b, code
  | (PAUnitSquare | PAQuarterCircle | PAHalfCircle | 
     PAFullCircle | PAName _ | PAKnot _) as p -> p, nop

and picture = function
  | PIMake c as p ->
      begin 
	try 
          let pic = Hashtbl.find known_pictures p in
	  PIName pic, nop
	with Not_found ->
	  let pic = Name.picture () in
	  Hashtbl.add known_pictures p pic;
	  PIName pic, CDefPic (pic, command c)
      end
  | PITransform (tr, p) ->
      let p, code = picture p in
      PITransform (tr, p), code
  | (PIName _ | PITex _)  as p -> p, nop

and box = function
  | BCircle (n, p, pic, s) ->
      let pic, code = picture pic in
      BCircle (n, p, pic, s), code
  | BRect (n, p, pic) ->
      let pic, code = picture pic in
      BRect (n, p, pic), code

and command = function
  | CDraw (p, color, pen, dash) ->
      let p, code = path p in
      CSeq [code; CDraw (p, color, pen, dash)]
  | CDrawPic p ->
      let p, code = picture p in
      CSeq [code; CDrawPic p]
  | CDrawBox (c, b) ->
      let b, code = box b in
      CSeq [code; CDrawBox (c, b)]
  | CSeq l ->
      CSeq (List.map command l)
  | CLoop (i, j, f) ->
      CLoop (i, j, fun k -> List.map command (f k))
  | c -> 
      c


let reset () = 
  Hashtbl.clear known_pictures
