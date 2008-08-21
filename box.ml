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
open Picture
open Point

open Num.Infix

type t = { c : point ; bpath : Shapes.t; pic : picture; height : num ; width : num }
type repr = t

let margin = Num.bp 2.

open Shapes

let rect_path pic dx dy = 
  (* construct rectangular path and return path + width + height *)
  let width = Picture.width pic +/ Num.two */ dx in
  let height = Picture.height pic +/ Num.two */ dy in
   Shapes.rectangle_path width height, width, height

let base_rect ?(dx=margin) ?(dy=margin) pic = 
  let c = Picture.ctr pic in
  let path, w, h = rect_path pic dx dy in
  { c = c; bpath = Shapes.shift c path ; pic = pic ; 
    height = h; width = w }

let rect ?(dx=margin) ?(dy=margin) c p = 
  let pic = Picture.center c p in
  let path,w, h = rect_path pic dx dy in
    { c = c; bpath = Shapes.shift c path; 
      pic = pic; width = w; height = h}


let circle ?(dx=margin) ?(dy=margin) c pic =
  let pic = center c pic in
  let r = length (add (pt (dx,dy)) (sub (urcorner pic) (llcorner pic))) in
  { c = c; pic = pic ; 
    height = r; width = r;
    bpath = Shapes.shift c { p= Path.scale r Path.fullcircle;
    b = let hr = 0.5 *./ r in Shapes.build_border hr hr }
  }

let ellipse ?(dx=F 0.) ?(dy=F 0.) c pic =
  let pic = center c pic in 
  let rx = length (sub (urcorner pic) (ulcorner pic)) in
  let ry = length (sub (urcorner pic) (lrcorner pic)) in
  let rx = rx +/ dx in
  let ry = ry +/ dy in
    { c = c; pic = pic; bpath = 
      Shapes.shift c (Shapes.full_ellipse_path rx ry); 
      height = ry */ Num.two ; width = rx */ Num.two }

let round_rect ?(dx=margin) ?(dy=margin) c p =
  let pic = center c p in
  let dx = length (sub (urcorner pic) (ulcorner pic)) +/ dx in
  let dy = length (sub (urcorner pic) (lrcorner pic)) +/ dy in
  let rx = min (dx /./ 10.) (dy /./ 10.) in
  { c = c; 
    bpath = Shapes.shift c (Shapes.rounded_rect_path dx dy rx rx); 
    pic = pic; width = dx; height = dy }
            
let patatoid ?(dx=2. *./ margin) ?(dy=2. *./ margin) c p =
  (* size is wrong for patatoids *)
  let pic = center c p in
  let w = Picture.width p in
  let h = Picture.height p in
  let path = Shapes.patatoid (w +/ 2. *./ dx) (h +/ 2. *./ dy) in
  let dummypic = Picture.make (Command.draw path.p) in
    { c = c; pic = pic; width = Picture.width dummypic; 
      height = Picture.height dummypic; bpath =  path}


let center {c = c} = c

let build_point a b = Point.pt (xpart a, ypart b)
let north_west x = build_point x.bpath.b.w x.bpath.b.n
let north_east x = build_point x.bpath.b.e x.bpath.b.n
let south_west x = build_point x.bpath.b.w x.bpath.b.s
let south_east x = build_point x.bpath.b.e x.bpath.b.s
let north x = x.bpath.b.n
let south x = x.bpath.b.s
let west  x = x.bpath.b.w
let east  x = x.bpath.b.e
(*
let north_west {pic = pic} = ulcorner pic
let north_east {pic = pic} = urcorner pic
let south_west {pic = pic} = llcorner pic
let south_east {pic = pic} = lrcorner pic

let north b = segment 0.5 (north_west b) (north_east b)
  
let south b = segment 0.5 (south_west b) (south_east b) 
let west  b = segment 0.5 (north_west b) (north_west b) 
let east  b = segment 0.5 (south_east b) (north_east b) 
*)

let bpath {bpath = p} = p.p

let picture p = p.pic

(* POS compliance *)


let v b = b

let width b = b.width

let height b = b.height

let ctr b = b.c

let shift p b = { b with c = Point.shift p b.c; 
                  pic = Picture.shift p b.pic;
		  bpath = Shapes.shift p b.bpath }

let center pt x = shift (Point.sub pt (ctr x)) (v x)

(* Box alignment *)

open Num.Infix

module PicAlign = Pos.List_ (Picture)
(* These functions should rather be called 
 * "align_block" or something like that *)
let valign ?(dx=Num.zero) ?(dy=Num.zero)  ?pos pl = 
  let posl = PicAlign.vertical ~dy  ?pos pl in
    List.map (fun p ->
      let dx = Point.xpart (Picture.ctr p) +/ dx -/
               Picture.width p /./ 2. in
        base_rect ~dx ~dy:(0.5 *./ dy) p)
    (PicAlign.v posl)

let halign ?(dx=Num.zero) ?(dy=Num.zero) ?pos pl =
  let posl = PicAlign.horizontal ~dx ?pos pl in
    List.map 
      (fun p ->
        let dy = Point.ypart (Picture.ctr p) +/ 
                 dy -/ Picture.height p /./ 2. in
        base_rect ~dx:(0.5 *./ dx) ~dy p)
      (PicAlign.v posl)

(* That is the function I would call halign  *)
let halign_to_box ?(dx=margin) ?(dy=margin) ?(spacing=Num.zero) ?pos pl =
  let posl = PicAlign.horizontal ~dx:(dx +/ spacing) ?pos pl in
    List.map (base_rect ~dx:(0.5 *./ dx) ~dy) (PicAlign.v posl)

open Command 

let draw ?fill ?(boxed=true) b = 
  let path_cmd = if boxed then draw b.bpath.p else nop in
  let box_cmd =
    match fill with
      | None -> draw_pic b.pic
      | Some color -> Command.fill ~color b.bpath.p ++ draw_pic b.pic
  in
    path_cmd ++ box_cmd

let tabularl ?(dx=Num.zero) ?(dy=Num.zero) pll =
  let hmaxl = List.map (Num.fold_max Picture.height Num.zero) pll in
  let rec calc_wmax pll = 
    match pll with 
      | []::_ -> [] 
      | _ -> let cols, qll = 
	  List.fold_left 
	    (fun (col,rem) pl -> (List.hd pl :: col, List.tl pl :: rem)) 
	    ([],[]) pll in
	  (Num.fold_max Picture.width Num.zero cols)::(calc_wmax qll)
  in
  let wmaxl = calc_wmax pll in
  let rec make_rows wmaxl x y h_2 pl =
    match pl, wmaxl with
      | [], [] -> []
      | [], _ | _, [] -> raise (Invalid_argument "Lists have different sizes")
      | p::ql, wrow :: wl ->
	  let c = Point.pt (x +/ dx +/ wrow /./ 2., y -/ h_2) in
	  let dx' = dx +/ (wrow -/ Picture.width p) /./ 2. in
	  let dy = h_2 -/ (Picture.height p) /./ 2. in
	  let b = rect ~dx:dx' ~dy c p in
	    b::(make_rows wl (x +/ wrow +/ dx +/ dx) y h_2 ql)
  in
  let rec make_array hmaxl y pll = 
    match pll, hmaxl with
      | [], [] -> []
      | [], _ | _, [] -> raise (Invalid_argument "Lists have different sizes")
      | row :: qll, hrow :: hl -> 
	  let brow = 
	    make_rows wmaxl Num.zero y (hrow /./ 2. +/ dy) row in
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

open Path

let cpath ?style ?outd ?ind a b =
  let r,l  = outd, ind in
  let p = pathk ?style [knotp ?r (ctr a); knotp ?l (ctr b)] in
    cut_after (bpath b) (cut_before (bpath a) p)

