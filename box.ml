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
open Num
open Point

open Num.Infix

open Pos

let margin = Num.bp 2.

module Make (P  : POS) =
struct
  (* A functor to construct boxes
   * It does nothing more than draw a Shape of the right size around a
   * positionable object. In particular, it does not reposition the object *)
  type 'a box = { p : 'a ; bs : Shapes.t ; c : Point.t ; 
             height : Num.t ; width : Num.t }
  type t = P.t box
  type repr = P.repr box
  let v x = { x with p = P.v x.p}

  let rect ?(dx=margin) ?(dy=margin) p = 
    let c = P.ctr p in
    let w = P.width p +/ 2. *./ dx in
    let h = P.height p +/ 2. *./ dy in
    let s = Shapes.shift c (Shapes.rectangle_path w h) in
    { c = c; p = p;  bs = s ; height = h; width = w }

  let circle ?(dx=margin) ?(dy=margin) p =
    let r = Point.length (add (pt (P.width p, P.height p)) (pt (dx,dy))) in
    let c = P.ctr p in
    let bpath = 
      Shapes.shift c 
        { Shapes.p= Path.scale r Path.fullcircle;
          b = let hr = 0.5 *./ r in Shapes.build_border hr hr }
    in
    { c = c; p = p; bs = bpath; height = r; width = r; }

  let ellipse ?(dx=zero) ?(dy=zero) p =
    let c = P.ctr p in 
    let rx = P.width p +/ dx in
    let ry = P.height p +/ dy in
      { c = c; p = p; bs = Shapes.shift c (Shapes.full_ellipse_path rx ry); 
        height = 2. *./ ry ; width = 2. *./ rx }

  let round_rect ?(dx=margin) ?(dy=margin) p =
    let c = P.ctr p in
    let dx = P.width p +/ dx in
    let dy = P.height p +/ dy in
    let rx = min (dx /./ 10.) (dy /./ 10.) in
    { c = c; p = p; bs = Shapes.shift c (Shapes.rounded_rect_path dx dy rx rx);
      width = dx; height = dy }
              
  let patatoid ?(dx=2. *./ margin) ?(dy=2. *./ margin) p =
    let c = P.ctr p in
    let w = P.width p in
    let h = P.height p in
    let path = Shapes.patatoid (w +/ 2. *./ dx) (h +/ 2. *./ dy) in
    let dummypic = Picture.make (Command.draw path.Shapes.p) in
    { c = c; p = p; bs = Shapes.shift c path; width = Picture.width dummypic; 
      height = Picture.height dummypic}


  let bshape b = b.bs
  let bpath b = Shapes.path b.bs

  let north x = Shapes.north (bshape x)
  let south x = Shapes.south (bshape x)
  let west  x = Shapes.west (bshape x)
  let east  x = Shapes.east (bshape x)

  let build_point a b = Point.pt (xpart a, ypart b)
  let north_west x = build_point (west x) (north x)
  let north_east x = build_point (east x) (north x)
  let south_west x = build_point (west x) (south x)
  let south_east x = build_point (east x) (south x)


  (* POS compliance *)

  let width b = b.width

  let height b = b.height

  let ctr b = b.c

  let shift pt x = 
    { x with p = P.shift pt x.p; bs = Shapes.shift pt x.bs;
      c = Point.shift pt x.c }

  let center pt x = shift (Point.sub pt (ctr x)) (v x)

end

module PicBox = Make (Picture)
include PicBox

(*define the "centering" box construction fonctions *)
let base_rect = rect
let rect ?dx ?dy c p = center c (rect ?dx ?dy p)
let circle ?dx ?dy c p = center c (circle ?dx ?dy p)
let round_rect ?dx ?dy c p = center c (round_rect ?dx ?dy p)
let ellipse ?dx ?dy c p = center c (ellipse ?dx ?dy p)
let patatoid ?dx ?dy c p = center c (patatoid ?dx ?dy p)

let picture b = b.PicBox.p


open Num.Infix

(* Box alignment *)
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
  let path_cmd = if boxed then draw (bpath b) else nop in
  let box_cmd =
    match fill with
      | None -> draw_pic (picture b)
      | Some color -> Command.fill ~color (bpath b) ++ draw_pic (picture b)
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

