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

module Smap = Map.Make(String)

module B = struct

  type t = {
    name : string option;
    width : Num.t;
    height : Num.t;
    ctr : Point.t;
    stroke : Color.t option;
    fill : Color.t option;
    contour : Shapes.t; 
    desc : desc
  }
      
  and desc = 
    | Pic of Picture.t
    | Grp of t array * t Smap.t

  type repr = t

  let v b = b

  let width b = b.width
  let height b = b.height
  let ctr b = b.ctr

  let rec shift pt b = 
    { b with 
      ctr = Point.shift pt b.ctr; 
      contour = Shapes.shift pt b.contour;
      desc = shift_desc pt b.desc }
      
  and shift_desc pt = function
    | Pic pic -> Pic (Picture.shift pt pic)
    | Grp (a, m) -> Grp (Array.map (shift pt) a, Smap.map (shift pt) m)

  let center pt x = shift (Point.sub pt x.ctr) x
  
end 

include B

let bshape b = b.contour
let bpath b = Shapes.bpath b.contour

let north x = Shapes.north (bshape x)
let south x = Shapes.south (bshape x)
let west  x = Shapes.west (bshape x)
let east  x = Shapes.east (bshape x)

let build_point a b = Point.pt (xpart a, ypart b)
let north_west x = build_point (west x) (north x)
let north_east x = build_point (east x) (north x)
let south_west x = build_point (west x) (south x)
let south_east x = build_point (east x) (south x)

let rec draw ?(debug=false) b = 
  let bpath = Shapes.bpath b.contour in
  let path_cmd = match b.stroke with
    | None -> Command.nop
    | Some color -> Command.draw ~color bpath 
  in
  let fill_cmd = match b.fill with
    | None -> Command.nop
    | Some color -> Command.fill ~color bpath
  in
  let contents_cmd = match b.desc with
    | Pic pic -> 
	Command.draw_pic pic
    | Grp (a, _) -> 
	Command.iter 0 (Array.length a - 1) (fun i -> draw ~debug a.(i))
  in
  let debug_cmd = 
    if debug then 
      Command.seq 
	[Command.draw ~color:Color.red ~dashed:Dash.evenly bpath;
	 match b.name with
	   | None -> Command.draw ~color:Color.red (Path.pathp [b.ctr])
	   | Some n -> 
	       Command.label ~pos:`Center (Picture.tex ("\\tiny " ^ n))
		 (north_west b)]
    else 
      Command.nop
  in
  Command.seq [fill_cmd; path_cmd; contents_cmd; debug_cmd]

type style = 
  | Rect | Circle | Ellipse | RoundRect | Patatoid

let rect ?(dx=margin) ?(dy=margin) w h c = 
  let w = w +/ 2. *./ dx in  let h = h +/ 2. *./ dy in
  let s = Shapes.shift c (Shapes.rectangle_path w h) in
  w, h, s

let circle ?(dx=margin) ?(dy=margin) w h c =
  let d = Point.length (add (pt (w, h)) (pt (dx, dy))) in
  let s = Shapes.shift c (Shapes.circle d) in
  d, d, s

let ellipse ?(dx=margin) ?(dy=margin) w h c =
  let rx = w /./ 2. +/ dx in
  let ry = h /./ 2. +/ dy in
  2. *./ rx, 2. *./ ry, Shapes.shift c (Shapes.full_ellipse_path rx ry) 

let round_rect_gen ?(dx=margin) ?(dy=margin) ?(rx=margin) ?(ry=margin) w h c =
  let dx = w +/ dx in
  let dy = h +/ dy in
  dx, dy, Shapes.shift c (Shapes.rounded_rect_path dx dy rx ry)

let round_rect ?(dx=margin) ?(dy=margin) w h c =
  let dx' = w +/ dx in
  let dy' = h +/ dy in
  let rx = (minn dx' dy') /./ 10. in
  round_rect_gen ~dx ~dy ~rx ~ry:rx w h c

let patatoid ?(dx=2. *./ margin) ?(dy=2. *./ margin) w h c =
  let path = Shapes.patatoid (w +/ 2. *./ dx) (h +/ 2. *./ dy) in
  let s = Shapes.shift c path in
  s.Shapes.wt, s.Shapes.ht, s

let make_contour style ?dx ?dy w h c =
  let f = match style with
    | Rect -> rect 
    | Circle -> circle
    | Ellipse -> ellipse
    | RoundRect -> round_rect
    | Patatoid -> patatoid
  in
  f ?dx ?dy w h c

let pic ?(style=Rect) ?dx ?dy ?name ?(stroke=Some Color.black) ?fill pic =
  let c = Picture.ctr pic in
  let w,h,s = 
    make_contour style ?dx ?dy (Picture.width pic) (Picture.height pic) c 
  in
  { desc = Pic pic;
    name = name; stroke = stroke; fill = fill;
    width = w; height = h; ctr = c; contour = s }

module BoxAlign = Pos.List_(B)

let merge_maps =
  let add_one m b =
    let m = match b.desc with
      | Pic _ -> m
      | Grp (_, m') -> Smap.fold Smap.add m' m
    in
    match b.name with Some n -> Smap.add n b m | None -> m
  in
  List.fold_left add_one Smap.empty

let align_boxes f ?padding ?pos ?(style=Rect) 
  ?(dx=Num.zero) ?(dy=Num.zero) ?name ?(stroke=None) ?fill bl =
  let bl = f ?padding ?pos bl in
  let w = BoxAlign.width bl in
  let h = BoxAlign.height bl in
  let c = BoxAlign.ctr bl in
  let w,h,s = make_contour style ~dx ~dy w h c in
  let bl = BoxAlign.v bl in
  { desc = Grp (Array.of_list bl, merge_maps bl);
    name = name; stroke = stroke; fill = fill;
    width = w; height = h; ctr = c; contour = s }

let hbox = align_boxes BoxAlign.horizontal
let vbox = align_boxes BoxAlign.vertical

let box 
  ?(style=Rect) ?(dx=margin) ?(dy=margin) ?name 
  ?(stroke=Some Color.black) ?fill b =
  hbox ~style ~dx ~dy ?name ~stroke ?fill [b]

let group 
  ?name ?(stroke=None) ?fill ?(style=Rect) ?(dx=Num.zero) ?(dy=Num.zero) bl =
  let xmin,xmax,ymin,ymax = 
    List.fold_left 
      (fun (xmin,xmax,ymin,ymax) b ->
	 let sw = south_west b in
	 let ne = north_east b in
	 (Num.minn xmin (xpart sw),
	  Num.maxn xmax (xpart ne),
	  Num.minn ymin (ypart sw),
	  Num.maxn ymax (ypart ne)))
      (Num.zero, Num.zero, Num.zero, Num.zero) 
      bl
  in
  let w = xmax -/ xmin in
  let h = ymax -/ ymin in
  let c = Point.pt (xmin +/ w /./ 2., ymin +/ h /./ 2.) in
  let w,h,s = make_contour style ~dx ~dy w h c in
  { desc = Grp (Array.of_list bl, merge_maps bl);
    name = name; stroke = stroke; fill = fill;
    width = w; height = h; ctr = c; contour = s }

type 'a box_creator = 
  ?dx:Num.t -> ?dy:Num.t -> ?name:string -> 
  ?stroke:Color.t option -> ?fill:Color.t -> 'a -> t

let rect = pic ~style:Rect
let circle = pic ~style:Circle
let ellipse = pic ~style:Ellipse
let round_rect = pic ~style:RoundRect
let patatoid = pic ~style:Patatoid

let tex ?style ?dx ?dy ?name ?stroke ?fill s = 
  pic ?style ?dx ?dy ?name ?stroke ?fill (Picture.tex s)

let nth i b = match b.desc with
  | Grp (a, _ ) -> a.(i)
  | Pic _ -> invalid_arg "Box.nth"

let elts b = match b.desc with
  | Pic _ -> [||]
  | Grp (a, _) -> a

let get n b = 
  if b.name = Some n then b else match b.desc with
    | Pic _ -> invalid_arg "Box.get"
    | Grp (_, m) -> try Smap.find n m with Not_found -> invalid_arg "Box.get"

let get_fill b = b.fill
let set_fill c b = {b with fill = Some c} 

(****

(* These functions should rather be called 
 * "align_block" or something like that *)
let valign ?(dx=Num.zero) ?(dy=Num.zero)  ?pos pl = 
  let posl = BoxAlign.vertical ~dy  ?pos pl in
  List.map 
    (fun p ->
      let dx = Point.xpart (Picture.ctr p) +/ dx -/
	Picture.width p /./ 2. 
      in
      rect ~dx ~dy:(0.5 *./ dy) p)
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

****)

let tabularl ?(hpadding=Num.zero) ?(vpadding=Num.zero) ?pos pll =
  let dx = hpadding and dy = vpadding in
  let hmaxl = List.map (Num.fold_max height Num.zero) pll in
  let rec calc_wmax pll = match pll with 
    | [] :: _ -> 
	[] 
    | _ -> 
	let cols, qll = 
	  List.fold_left 
	    (fun (col,rem) pl -> (List.hd pl :: col, List.tl pl :: rem)) 
	    ([],[]) pll 
	in
	(Num.fold_max width Num.zero cols) :: (calc_wmax qll)
  in
  let wmaxl = calc_wmax pll in
  let rec make_rows wmaxl x y h_2 pl = match pl, wmaxl with
    | [], [] -> []
    | [], _ | _, [] -> raise (Invalid_argument "Lists have different sizes")
    | p::ql, wrow :: wl ->
	let c = Point.pt (x +/ dx +/ wrow /./ 2., y -/ h_2) in
	let dx' = dx +/ (wrow -/ width p) /./ 2. in
	let dy = h_2 -/ (height p) /./ 2. in
	let b = shift c p in
	b :: (make_rows wl (x +/ wrow +/ dx +/ dx) y h_2 ql)
  in
  let rec make_array hmaxl y pll = match pll, hmaxl with
    | [], [] -> []
    | [], _ | _, [] -> raise (Invalid_argument "Lists have different sizes")
    | row :: qll, hrow :: hl -> 
	let brow = 
	  group (make_rows wmaxl Num.zero y (hrow /./ 2. +/ dy) row)
	in
	brow :: (make_array hl (y -/ hrow -/ dy -/ dy) qll)
  in
  group (make_array hmaxl Num.zero pll)

let tabular ?(hpadding=Num.zero) ?(vpadding=Num.zero) ?pos m =
  let pll = Array.to_list (Array.map Array.to_list m) in
  tabularl ~hpadding ~vpadding ?pos pll

let tabulari ?(hpadding=Num.zero) ?(vpadding=Num.zero) ?pos w h f =
  let m = Array.init h (fun j -> Array.init w (fun i -> f i j)) in
  tabular ~hpadding ~vpadding ?pos m

open Path

let cpath ?style ?outd ?ind a b =
  let r,l = outd, ind in
  let p = pathk ?style [knotp ?r (ctr a); knotp ?l (ctr b)] in
  cut_after (bpath b) (cut_before (bpath a) p)
