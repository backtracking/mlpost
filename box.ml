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

let fold_max f = List.fold_left (fun w p -> Num.maxn w (f p))

let valign ?(dx=Num.zero) ?(dy=Num.zero) pl = 
  let wmax = fold_max Picture.width Num.zero pl in
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

let halign ?(dx=Num.zero) ?(dy=Num.zero) pl = 
  let hmax = fold_max Picture.height Num.zero pl in
  let hmax_2 = hmax // Num.two +/ dy in
  let rec make_boxes x = function
    | [] -> 
	[]
    | p :: pl ->
	let wp = Picture.width p in
	let hp = Picture.height p in
	let dy = hmax_2 -/ hp // Num.two in
	let c = Point.pt (x +/ dx +/ wp // Num.two, hmax_2) in 
	let b = rect ~dx ~dy c p in
	b :: make_boxes (x +/ wp +/ dx +/ dx) pl
  in
  make_boxes Num.zero pl

let tabularl ?(dx=Num.zero) ?(dy=Num.zero) pll =
  let hmaxl = List.map (fold_max Picture.height Num.zero) pll in
  let rec calc_wmax pll = 
    match pll with 
      | []::_ -> [] 
      | _ -> let cols, qll = 
	  List.fold_left 
	    (fun (col,rem) pl -> (List.hd pl :: col, List.tl pl :: rem)) 
	    ([],[]) pll in
	  (fold_max Picture.width Num.zero cols)::(calc_wmax qll)
  in
  let wmaxl = calc_wmax pll in
  let rec make_rows wmaxl x y h_2 pl =
    match pl, wmaxl with
      | [], [] -> []
      | [], _ | _, [] -> raise (Invalid_argument "Lists have different sizes")
      | p::ql, wrow :: wl ->
	  let c = Point.pt (x +/ dx +/ wrow // Num.two, y -/ h_2) in
	  let dx' = dx +/ (wrow -/ Picture.width p) // Num.two in
	  let dy = h_2 -/ (Picture.height p) // Num.two in
	  let b = rect ~dx:dx' ~dy c p in
	    b::(make_rows wl (x +/ wrow +/ dx +/ dx) y h_2 ql)
  in
  let rec make_array hmaxl y pll = 
    match pll, hmaxl with
      | [], [] -> []
      | [], _ | _, [] -> raise (Invalid_argument "Lists have different sizes")
      | row :: qll, hrow :: hl -> 
	  let brow = 
	    make_rows wmaxl Num.zero y (hrow // Num.two +/ dy) row in
	    brow :: (make_array hl (y -/ hrow -/ dy -/ dy) qll)
  in
    make_array hmaxl Num.zero pll

let tabular ?(dx=Num.zero) ?(dy=Num.zero) m =
  let pll = Array.to_list (Array.map Array.to_list m) in
  let bll = tabularl ~dx ~dy pll in
    Array.of_list (List.map Array.of_list bll)

let tabulari ?(dx=Num.zero) ?(dy=Num.zero) w h f =
  let m = Array.init h (fun j -> Array.init w (fun i -> f i j)) in
    tabular ~dx ~dy m
