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
    pen : Pen.t option;
    fill : Color.t option;
    contour : Shapes.t; 
    desc : desc
  }
      
  and desc = 
    | Emp
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
    | Emp -> Emp
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
  let path_cmd = match b.stroke, b.pen with
    | None, _ -> Command.nop
    | Some color, None -> Command.draw ~color bpath 
    | Some color, Some pen -> Command.draw ~pen ~color bpath
  in
  let fill_cmd = match b.fill with
    | None -> Command.nop
    | Some color -> Command.fill ~color bpath
  in
  let contents_cmd = match b.desc with
    | Emp ->
	Command.nop
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

let pic ?(style=Rect) ?dx ?dy ?name ?(stroke=Some Color.black) ?pen ?fill pic =
  let c = Picture.ctr pic in
  let w,h,s = 
    make_contour style ?dx ?dy (Picture.width pic) (Picture.height pic) c 
  in
  { desc = Pic pic;
    name = name; stroke = stroke; pen = pen; fill = fill;
    width = w; height = h; ctr = c; contour = s }

module BoxAlign = Pos.List_(B)

let merge_maps =
  let add_one m b =
    let m = match b.desc with
      | Emp | Pic _ -> m
      | Grp (_, m') -> Smap.fold Smap.add m' m
    in
    match b.name with Some n -> Smap.add n b m | None -> m
  in
  List.fold_left add_one Smap.empty

let align_boxes f ?padding ?pos ?(style=Rect) 
  ?(dx=Num.zero) ?(dy=Num.zero) ?name ?(stroke=None) ?pen ?fill bl =
  let bl = f ?padding ?pos bl in
  let w = BoxAlign.width bl in
  let h = BoxAlign.height bl in
  let c = BoxAlign.ctr bl in
  let w,h,s = make_contour style ~dx ~dy w h c in
  let bl = BoxAlign.v bl in
  { desc = Grp (Array.of_list bl, merge_maps bl);
    name = name; stroke = stroke; pen = pen; fill = fill;
    width = w; height = h; ctr = c; contour = s }

let hbox = align_boxes BoxAlign.horizontal
let vbox = align_boxes BoxAlign.vertical

let box 
  ?(style=Rect) ?(dx=margin) ?(dy=margin) ?name 
  ?(stroke=Some Color.black) ?pen ?fill b =
  hbox ~style ~dx ~dy ?name ~stroke ?pen ?fill [b]

(* groups the given boxes in a new box *)
let group 
  ?name ?(stroke=None) ?pen ?fill ?(style=Rect) 
  ?(dx=Num.zero) ?(dy=Num.zero) bl =
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
    name = name; stroke = stroke; pen = pen; fill = fill;
    width = w; height = h; ctr = c; contour = s }

let group_array ?name ?stroke ?fill ?dx ?dy ba =
  group ?name ?stroke ?fill ?dx ?dy (Array.to_list ba)

(* groups the given boxes in a rectangular shape of size [w,h]
   and center [c] *)
let group_rect w h c bl =
  { desc = Grp (Array.of_list bl, merge_maps bl);
    name = None; stroke = None; pen = None; fill = None;
    width = w; height = h; ctr = c; 
    contour = Shapes.center c (Shapes.rectangle_path w h) }

let empty ?(width=Num.zero) ?(height=Num.zero) () =
  { desc = Emp; name = None; 
    stroke = None; pen = None; fill = None;
    width = width; height = height; ctr = Point.origin;
    contour = Shapes.rectangle_path width height }

type 'a box_creator = 
  ?dx:Num.t -> ?dy:Num.t -> ?name:string -> 
  ?stroke:Color.t option -> ?pen:Pen.t -> ?fill:Color.t -> 'a -> t

let rect = pic ~style:Rect
let circle = pic ~style:Circle
let ellipse = pic ~style:Ellipse
let round_rect = pic ~style:RoundRect
let patatoid = pic ~style:Patatoid

let tex ?style ?dx ?dy ?name ?stroke ?pen ?fill s = 
  pic ?style ?dx ?dy ?name ?stroke ?pen ?fill (Picture.tex s)

let nth i b = match b.desc with
  | Grp (a, _ ) -> a.(i)
  | Emp | Pic _ -> invalid_arg "Box.nth"

let elts b = match b.desc with
  | Emp | Pic _ -> [||]
  | Grp (a, _) -> a

let elts_list b = Array.to_list (elts b)

let get n b = 
  if b.name = Some n then b else match b.desc with
    | Emp | Pic _ -> invalid_arg "Box.get"
    | Grp (_, m) -> try Smap.find n m with Not_found -> invalid_arg "Box.get"

let get_fill b = b.fill
let set_fill c b = { b with fill = Some c } 

let get_stroke b = b.stroke
let set_stroke s b = {b with stroke = Some s } 
let clear_stroke b = { b with stroke = None }
let get_name b = b.name
let set_name name b = {b with name = Some name}

let get_pen b = b.pen
let set_pen p b = { b with pen = Some p }

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

let tabularl ?(hpadding=Num.zero) ?(vpadding=Num.zero) ?(pos=`Center) pll =
  if pll = [] then invalid_arg "Box.tabular: empty list";
  let len = List.length (List.hd pll) in
  if List.exists (fun l -> List.length l <> len) pll then 
    invalid_arg "Box.tabular: lists have different lengths";
  let dx = hpadding and dy = vpadding in
  (* we first compute the widths of columns and heights of rows *)
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
  let tw = List.fold_left (fun a w -> a +/ w +/ dx) Num.zero wmaxl -/ dx in
  let tw_2 = tw /./ 2. in
  let th = List.fold_left (fun a h -> a +/ h +/ dy) Num.zero hmaxl -/ dy in
  (* place the box [b] inside the rectangular region defined by upper left
     corner [x,y], width [w] and height [h], according to alignment [pos] *)
  let place_box x y w h b =
    let xb = match pos with
      | `Center | `Top | `Bot -> x +/ w /./ 2.
      | `Left | `Upleft | `Lowleft -> x +/ width b /./ 2.
      | `Right | `Upright | `Lowright -> x +/ w -/ width b /./ 2.
    in
    let yb = match pos with
      | `Center | `Left | `Right -> y -/ h /./ 2.
      | `Top | `Upright | `Upleft -> y -/ height b /./ 2.
      | `Bot | `Lowleft | `Lowright -> y -/ h +/ height b /./ 2.
    in
    center (Point.pt (xb, yb)) b
  in
  (* make a single row with upper left corner [x,y] and height [hrow] *)
  let rec make_row x y hrow wmaxl pl = match pl, wmaxl with
    | [], [] -> []
    | [], _ | _, [] -> assert false
    | p :: ql, wrow :: wl ->
	let b = place_box x y wrow hrow p in
	b :: make_row (x +/ wrow +/ dx) y hrow wl ql
  in
  (* make all rows, with upper left corner [0,y] *)
  let rec make_array hmaxl y pll = match pll, hmaxl with
    | [], [] -> []
    | [], _ | _, [] -> assert false
    | row :: qll, hrow :: hl -> 
	let brow = 
	  let c = Point.pt (tw_2, y -/ hrow /./ 2.) in
	  group_rect tw hrow c (make_row Num.zero y hrow wmaxl row)
	in
	brow :: make_array hl (y -/ hrow -/ dy) qll
  in
  let c = Point.pt (tw_2, Num.neg (th /./ 2.)) in
  group_rect tw th c (make_array hmaxl Num.zero pll)

let tabular ?(hpadding=Num.zero) ?(vpadding=Num.zero) ?pos m =
  let pll = Array.to_list (Array.map Array.to_list m) in
  tabularl ~hpadding ~vpadding ?pos pll

let tabulari ?(hpadding=Num.zero) ?(vpadding=Num.zero) ?pos w h f =
  let m = Array.init h (fun j -> Array.init w (fun i -> f i j)) in
  tabular ~hpadding ~vpadding ?pos m

(* blocks *)

let hblock ?(pos=`Center) pl =
  let hmax = Num.fold_max height Num.zero pl in
  let hmax_2 = hmax /./ 2. in
  let rec make_new acc x = function
    | [] -> 
	List.rev acc, x
    | p :: pl ->
        let wp,hp = width p, height p in
        let y = match pos with
          | `Center -> hmax_2
          | `Top -> hmax -/ hp /./ 2.
          | `Bot -> hp /./ 2.
        in
	let xc = x +/ wp /./ 2. in
        let c = Point.pt (xc, y) in 
        let b = center c p in
	let b = group_rect wp hmax (Point.pt (xc, hmax_2)) [b] in
	let b = set_stroke Color.black b in
        make_new (b::acc) (x +/ wp) pl
  in
  let l,x = make_new [] Num.zero pl in
  let mycenter = Point.pt (x /./ 2., hmax_2) in     
  group_rect x hmax mycenter l

let vblock ?(pos=`Center) pl =
  let wmax = Num.fold_max width Num.zero pl in
  let wmax_2 = wmax /./ 2. in
  let rec make_new acc y = function
    | [] -> 
	List.rev acc, y
    | p :: pl ->
        let wp,hp = width p, height p in
        let x = match pos with
          | `Center -> wmax_2
          | `Right -> wmax -/ wp /./ 2.
          | `Left ->  wp /./ 2.
        in
	let yc = y -/ hp /./ 2. in
        let c = Point.pt (x, yc) in 
        let b = center c p in
	let b = group_rect wmax hp (Point.pt (wmax_2, yc)) [b] in
	let b = set_stroke Color.black b in
        make_new (b::acc) (y -/ hp) pl
  in
  let l,y = make_new [] Num.zero pl in
  let mycenter = Point.pt (wmax_2, y /./ 2.) in
  group_rect wmax (Num.neg y) mycenter l

open Path

let cpath ?style ?outd ?ind a b =
  let r,l = outd, ind in
  let p = pathk ?style [knotp ?r (ctr a); knotp ?l (ctr b)] in
  cut_after (bpath b) (cut_before (bpath a) p)
