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
open Picture
open Point

open Num.Infix

type t = box

let margin = Num.bp 2.

let rect ?(dx=margin) ?(dy=margin) c p = 
  let pic = center p c in
  let pdx = Point.pt (dx, Num.zero) in
  let pdy = Point.pt (Num.zero, dy) in
  let path = 
    Path.pathp ~style:JLine ~cycle:JLine 
      [Point.add (Point.sub (ulcorner pic) pdx) pdy;
       Point.sub (Point.sub (llcorner pic) pdx) pdy;
       Point.sub (Point.add (lrcorner pic) pdx) pdy;
       Point.add (Point.add (urcorner pic) pdx) pdy]
  in
  { c = c; bpath = path ; pic = pic }

let circle ?(dr=F 0.) c pic =
  let pic = center pic c in
  let r = length (sub (urcorner pic) (llcorner pic)) in
  let r = r +/ margin +/ dr in
  { c = c; pic = pic ; bpath = Path.shift c (Path.scale r Path.fullcircle)}

let ellipse ?(dx=F 0.) ?(dy=F 0.) c pic =
  let pic = center pic c in 
  let rx = length (sub (urcorner pic) (ulcorner pic)) in
  let ry = length (sub (urcorner pic) (lrcorner pic)) in
  let rx = rx +/ dx in
  let ry = ry +/ dy in
    { c = c; pic = pic; bpath = Path.shift c (Shapes.full_ellipse_path rx ry) }

let round_rect ?(dx=margin) ?(dy=margin) c p =
  let pic = center p c in
  let dx = length (sub (urcorner pic) (ulcorner pic)) +/ dx in
  let dy = length (sub (urcorner pic) (lrcorner pic)) +/ dy in
  let rx = min (dx // (F 10.)) (dy // (F 10.)) in
  { c = c; 
    bpath = Path.shift c (Shapes.rounded_rect_path dx dy rx rx); 
    pic = pic }
            


let center {c = c} = c

let north_west {pic = pic} = ulcorner pic
let north_east {pic = pic} = urcorner pic
let south_west {pic = pic} = llcorner pic
let south_east {pic = pic} = lrcorner pic

let north b = segment 0.5 (north_west b) (north_east b)
  
let south b = segment 0.5 (south_west b) (south_east b) 
let west  b = segment 0.5 (north_west b) (north_west b) 
let east  b = segment 0.5 (south_east b) (north_east b) 

let bpath {bpath = p} = p

let picture p = p.pic


(* Box alignment *)

open Num.Infix

let valign ?(dx=Num.zero) ?(dy=Num.zero) pl = 
  let wmax = 
    List.fold_left (fun w p -> Num.maxn w (Picture.width p)) Num.zero pl 
  in
  let wmax_2 = wmax // Num.two +/ dx in
  let rec make_boxes y = function
    | [] -> 
	[]
    | p :: pl ->
	let wp = Picture.width p in
	let hp = Picture.height p in
	let dx = wmax_2 -/ wp // Num.two in
	let c = Point.pt (wmax_2, y -/ dy -/ hp // Num.two) in 
	let b = rect ~dx ~dy c p in
	b :: make_boxes (y -/ hp -/ dy -/ dy) pl
  in
  make_boxes Num.zero pl
