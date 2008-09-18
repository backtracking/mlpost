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
type repr = t

let tex s = mkPITex s
let make l = mkPIMake l
let bbox pic = mkPABBox pic
let ulcorner pic = mkPTPicCorner pic UL
let llcorner pic = mkPTPicCorner pic LL
let urcorner pic = mkPTPicCorner pic UR
let lrcorner pic = mkPTPicCorner pic LR

let corner_bbox ?(dx=Num.zero) ?(dy=Num.zero) pic = 
  let pdx = Point.pt (dx, Num.zero) in
  let pdy = Point.pt (Num.zero, dy) in
  Path.pathp ~style:PrimPath.jLine ~cycle:PrimPath.jLine 
    [Point.add (Point.sub (ulcorner pic) pdx) pdy;
     Point.sub (Point.sub (llcorner pic) pdx) pdy;
     Point.sub (Point.add (lrcorner pic) pdx) pdy;
     Point.add (Point.add (urcorner pic) pdx) pdy]

let transform tr p = 
  match p.Hashcons.node with
  | PITransform (tr', p) -> mkPITransform (tr'@tr) p
  | _ -> mkPITransform tr p

let ctr pic = Point.segment 0.5 (llcorner pic) (urcorner pic)

let center p pic = 
    transform [Transform.shifted (Point.sub p (ctr pic))] pic

let place_up_left pic p =
  transform [Transform.shifted (Point.sub p (ulcorner pic))] pic

let place_up_right pic p =
  transform [Transform.shifted (Point.sub p (urcorner pic))] pic

let place_bot_left pic p =
  transform [Transform.shifted (Point.sub p (llcorner pic))] pic

let shift pt pic = transform [Transform.shifted pt] pic

let place_bot_right pic p =
  transform [Transform.shifted (Point.sub p (lrcorner pic))] pic
let beside p1 p2 = 
  make (mkCSeq [mkCDrawPic p1; mkCDrawPic (place_up_left p2 (urcorner p1))])

let below p1 p2 =
  make (mkCSeq [mkCDrawPic p1; mkCDrawPic (place_up_left p2 (llcorner p1))])

let clip pic pth = mkPIClip pic pth

let width p = Point.xpart (Point.sub (urcorner p) (ulcorner p))

let height p = Point.ypart (Point.sub (urcorner p) (lrcorner p))

let scale f p = transform [Transform.scaled f] p
let rotate f p = transform [Transform.rotated f] p
let shift pt p = transform [Transform.shifted pt] p
let yscale n p = transform [Transform.yscaled n] p
let xscale n p = transform [Transform.xscaled n] p

let spin f p = transform [Transform.rotate_around (ctr p) f] p

let v x = x
