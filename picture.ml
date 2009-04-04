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

let tex s = mkPITex s
let make l = mkPIMake l
let empty = mkPIMake (mkCSeq [])
let bbox pic = mkPABBox pic

let ulcorner pic = mkPTPicCorner pic `Upleft
let llcorner pic = mkPTPicCorner pic `Lowleft
let urcorner pic = mkPTPicCorner pic `Upright
let lrcorner pic = mkPTPicCorner pic `Lowright

let north_west = ulcorner
let south_west = llcorner
let north_east = urcorner
let south_east = lrcorner

let corner_bbox ?(dx=Num.zero) ?(dy=Num.zero) pic = 
  let pdx = Point.pt (dx, Num.zero) in
  let pdy = Point.pt (Num.zero, dy) in
  Path.pathp ~style:Path.jLine ~cycle:Path.jLine 
    [Point.add (Point.sub (ulcorner pic) pdx) pdy;
     Point.sub (Point.sub (llcorner pic) pdx) pdy;
     Point.sub (Point.add (lrcorner pic) pdx) pdy;
     Point.add (Point.add (urcorner pic) pdx) pdy]

let transform trl p = List.fold_left mkPITransformed p trl 

let ctr pic = Point.segment 0.5 (llcorner pic) (urcorner pic)

let scale f p = transform [Transform.scaled f] p
let rotate f p = transform [Transform.rotated f] p
let shift pt p = transform [Transform.shifted pt] p
let yscale n p = transform [Transform.yscaled n] p
let xscale n p = transform [Transform.xscaled n] p

let spin f p = transform [Transform.rotate_around (ctr p) f] p

let place f pic p = shift (Point.sub p (f pic)) pic
let center p pic = place ctr pic p
let place_up_left p pic = place ulcorner pic p
let place_up_right p pic = place urcorner pic p
let place_bot_left p pic = place llcorner pic p
let place_bot_right p pic = place lrcorner pic p

let beside p1 p2 = 
  make (mkCSeq [mkCDrawPic p1; mkCDrawPic (place_up_left (urcorner p1) p2)])

let below p1 p2 =
  make (mkCSeq [mkCDrawPic p1; mkCDrawPic (place_up_left (llcorner p1) p2)])

let clip pic pth = mkPIClip pic pth

let width p = Point.xpart (Point.sub (urcorner p) (ulcorner p))

let height p = Point.ypart (Point.sub (urcorner p) (lrcorner p))

let north p = Point.segment 0.5 (north_east p) (north_west p)
let south p = Point.segment 0.5 (south_east p) (south_west p)
let west p = Point.segment 0.5 (south_west p) (north_west p)
let east p = Point.segment 0.5 (north_east p) (south_east p)

let corner pos x = 
  match pos with
  | `Upleft -> north_west x
  | `Upright -> north_east x
  | `Lowleft -> south_west x
  | `Lowright -> south_east x
  | `Left -> west x
  | `Right -> east x
  | `Center -> ctr x
  | `Top -> north x
  | `Bot -> south x
