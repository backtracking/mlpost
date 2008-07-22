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

open Types

type t = picture

let tex s = PITex s
let make l = PIMake l
let bbox pic = PABBox pic
let ulcorner pic = PTPicCorner (pic, UL)
let llcorner pic = PTPicCorner (pic, LL)
let urcorner pic = PTPicCorner (pic, UR)
let lrcorner pic = PTPicCorner (pic, LR)
(* let currentpicture = PIName "currentpicture" *)

let corner_bbox ?(dx=Num.zero) ?(dy=Num.zero) pic = 
  let pdx = Point.pt (dx, Num.zero) in
  let pdy = Point.pt (Num.zero, dy) in
  Path.pathp ~style:JLine ~cycle:JLine 
    [Point.add (Point.sub (ulcorner pic) pdx) pdy;
     Point.sub (Point.sub (llcorner pic) pdx) pdy;
     Point.sub (Point.add (lrcorner pic) pdx) pdy;
     Point.add (Point.add (urcorner pic) pdx) pdy]

let transform tr = function
  | PITransform (tr', p) -> PITransform (tr'@tr, p)
  | _ as x -> PITransform (tr, x)

let center pic p = 
  let bp = Point.segment 0.5 (llcorner pic) (urcorner pic) in
    transform [Transform.shifted (Point.sub p bp)] pic

let place_up_left pic p =
  transform [Transform.shifted (Point.sub p (ulcorner pic))] pic

let place_up_right pic p =
  transform [Transform.shifted (Point.sub p (urcorner pic))] pic

let place_bot_left pic p =
  transform [Transform.shifted (Point.sub p (llcorner pic))] pic

let place_bot_right pic p =
  transform [Transform.shifted (Point.sub p (lrcorner pic))] pic
let beside p1 p2 = 
  make (CSeq [CDrawPic p1; CDrawPic (place_up_left p2 (urcorner p1))])

let below p1 p2 =
  make (CSeq [CDrawPic p1; CDrawPic (place_up_left p2 (llcorner p1))])

let clip pic pth = PIClip (pic,pth)

let width p = Point.xpart (Point.sub (urcorner p) (ulcorner p))

let height p = Point.ypart (Point.sub (urcorner p) (lrcorner p))
