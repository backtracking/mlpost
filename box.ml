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

type style = 
  | Rect
  | Circle
  | RoundRect
  | Patatoid
  | Ellipse
  | Custom of (Num.t -> Num.t -> Num.t * Num.t * Path.t)

let margin = Num.bp 2.

module Smap = Map.Make(String)

type t = {
  name : string option;
  width : Num.t;
  height : Num.t;
  ctr : Point.t;
  stroke : Color.t option;
  pen : Pen.t option;
  fill : Color.t option;
  contour : Path.t; 
  desc : desc;
  post_draw : t -> Command.t ;
  pre_draw : t -> Command.t
}
    
and desc = 
  | Emp
  | Pic of Picture.t
  | Grp of t array * t Smap.t

let width b = b.width
let height b = b.height
let ctr b = b.ctr

let rec shift pt b = 
  { b with 
    ctr = Point.shift pt b.ctr; 
    contour = Path.shift pt b.contour;
    desc = shift_desc pt b.desc }
    
and shift_desc pt = function
  | Emp -> Emp
  | Pic pic -> Pic (Picture.shift pt pic)
  | Grp (a, m) -> Grp (Array.map (shift pt) a, Smap.map (shift pt) m)

let center pt x = shift (Point.sub pt x.ctr) x

let bpath b = b.contour

let halfheight b = Point.pt (zero, b.height /./ 2.)
let halfwidth b = Point.pt (b.width /./ 2., zero)
let north b = Point.add b.ctr (halfheight b)
let south b = Point.sub b.ctr (halfheight b)
let east b = Point.add b.ctr (halfwidth b)
let west b = Point.sub b.ctr (halfwidth b)

let build_point a b = Point.pt (xpart a, ypart b)
let north_west x = build_point (west x) (north x)
let north_east x = build_point (east x) (north x)
let south_west x = build_point (west x) (south x)
let south_east x = build_point (east x) (south x)

let rec draw ?(debug=false) b = 
  let path_cmd = match b.stroke, b.pen with
    | None, _ -> Command.nop
    | Some color, None -> Command.draw ~color b.contour
    | Some color, Some pen -> Command.draw ~pen ~color b.contour
  in
  let fill_cmd = match b.fill with
    | None -> Command.nop
    | Some color -> Command.fill ~color b.contour
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
	[Command.draw ~color:Color.red ~dashed:Dash.evenly b.contour;
	 match b.name with
	   | None -> Command.draw ~color:Color.red (Path.pathp [b.ctr])
	   | Some n -> 
	       Command.label ~pos:`Center (Picture.tex ("\\tiny " ^ n))
		 (north_west b)]
    else 
      Command.nop
  in
  Command.seq [b.pre_draw b; fill_cmd; contents_cmd; path_cmd; debug_cmd; 
               b.post_draw b]

let rect_ w h = w, h, Shapes.rectangle w h
let circ_ w h = 
  let m = maxn w h in
  m, m, Shapes.circle m
let ellipse_ w h = w, h, Shapes.ellipse w h
let round_rect_ w h = 
  let rx = (minn w h) /./ 10. in
  w, h, Shapes.round_rect w h rx rx
let patatoid_ w h = 
  let p = Shapes.patatoid w h in
  let pic = Picture.make (Command.draw p) in
  Picture.width pic, Picture.height pic, p

let from_style = function
  | Rect -> rect_
  | Circle -> circ_
  | RoundRect -> round_rect_
  | Patatoid -> patatoid_
  | Ellipse -> ellipse_
  | Custom f -> f

let make_contour s ?(dx=margin) ?(dy=margin) w h c =
  let w =  w +/ 2. *./ dx and h = h +/ 2. *./ dy in
  let w,h, p = (from_style s) w h in
  w, h, Path.shift c p

let no_drawing _ = Command.nop

let pic ?(style=Rect) ?dx ?dy ?name ?(stroke=Some Color.black) 
        ?pen ?fill pic =
  let c = Picture.ctr pic in
  let w,h,s = 
    make_contour style ?dx ?dy (Picture.width pic) (Picture.height pic) c 
  in
  { desc = Pic pic;
    name = name; stroke = stroke; pen = pen; fill = fill;
    width = w; height = h; ctr = c; contour = s;
    post_draw = no_drawing; pre_draw = no_drawing }

let box ?(style=Rect) ?dx ?dy ?name ?(stroke=Some Color.black) 
        ?pen ?fill b =
  let c = ctr b in
  let w,h,s = 
    make_contour style ?dx ?dy (width b) (height b) c 
  in
  { desc = Grp ([|b|], Smap.empty) ;
    name = name; stroke = stroke; pen = pen; fill = fill;
    width = w; height = h; ctr = c; contour = s;
    post_draw = no_drawing; pre_draw = no_drawing }

let merge_maps =
  let add_one m b =
    let m = match b.desc with
      | Emp | Pic _ -> m
      | Grp (_, m') -> Smap.fold Smap.add m' m
    in
    match b.name with Some n -> Smap.add n b m | None -> m
  in
  List.fold_left add_one Smap.empty

let path ?style ?dx ?dy ?name ?(stroke=None) ?pen ?fill p = 
  pic ?style ?dx ?dy ?name ~stroke ?pen ?fill
    (Picture.make (Command.draw p))

let border pos b = 
  match pos with
  | `Top -> ypart (ctr b) +/ height b /./ 2.
  | `Bot -> ypart (ctr b) -/ height b /./ 2.
  | `Left -> xpart (ctr b) -/ width b /./ 2.
  | `Right -> xpart (ctr b) +/ width b /./ 2.

let diffy pos a b =
  (* calculate the vertical difference between two boxes, taking either the
   * center, the top or the bottom as reference  *)
  let diffc = ypart (Point.sub (ctr a) (ctr b)) in
  match pos with
  | `Center -> diffc 
  | (`Top | `Bot) as x -> border x a -/ border x b

let diffx pos a b =
  (* calculate the horizontal difference between two boxes, taking either the
   * center, the top or the bottom as reference  *)
  let diffc = xpart (Point.sub (ctr a) (ctr b)) in
  match pos with
  | `Center -> diffc 
  | (`Left | `Right) as x -> border x a -/ border x b


let horizontal ?(padding=Num.zero) ?(pos=`Center) pl =
  let refb = 
    try List.hd pl 
    with Failure "hd" -> raise (invalid_arg "hbox - empty list") 
  in
  let refc = ctr refb and refw = width refb in
  let rec make_new acc x = function
    | [] -> List.rev acc, x -/ padding
    | p :: pl ->
        let diffy = diffy pos refb p in
        let offsetx = x +/ width p /./ 2. in
        let b = shift (Point.pt (offsetx -/ xpart (ctr p), diffy)) p in
        make_new (b::acc) (x +/ width p +/ padding) pl
  in
  let l, x = make_new [] (xpart refc -/ refw /./ 2.) pl in
  let x = x  -/ xpart refc +/ refw /./ 2. in
  let hmax = Num.fold_max height Num.zero pl in
  let cy = 
    match pos with
    | `Center -> ypart refc
    | `Top -> border `Top  refb -/ hmax /./ 2.
    | `Bot -> border `Bot  refb +/ hmax /./ 2.
  in
  let mycenter = Point.pt (border `Left refb +/ x /./ 2.,  cy) in     
  l, x, hmax, mycenter

let vertical ?(padding=Num.zero) ?(pos=`Center) pl =
  let refb = 
    try List.hd pl 
    with Failure "hd" -> raise (invalid_arg "vbox - empty list") 
  in
  let refc = ctr refb and refh = height refb in
  let rec make_new acc y = function
    | [] -> List.rev acc, y +/ padding
    | p :: pl ->
        let diffx = diffx pos refb p in
        let offsety = y -/ height p /./ 2. in
        let b = shift (Point.pt (diffx, offsety -/ ypart (ctr p))) p in
        make_new (b::acc) (y -/ height p -/ padding) pl 
  in
  let wmax = Num.fold_max width Num.zero pl in
  let l,y = make_new [] (ypart refc +/ refh /./ 2.) pl in
  let y = y -/ ypart refc -/ refh /./ 2. in
  let cx = 
    match pos with
    | `Center -> xpart refc
    | `Left -> border `Left refb +/ wmax /./ 2.
    | `Right -> border `Right refb -/ wmax /./ 2.
  in
  let mycenter = Point.pt (cx, border `Top refb +/ y /./ 2.) in
  l, wmax, Num.neg y, mycenter

let align_boxes f ?padding ?pos ?(style=Rect) 
  ?(dx=Num.zero) ?(dy=Num.zero) ?name ?(stroke=None) ?pen ?fill bl =
  let bl, w, h , c = f ?padding ?pos bl in
  let w,h,s = make_contour style ~dx ~dy w h c in
  { desc = Grp (Array.of_list bl, merge_maps bl);
    name = name; stroke = stroke; pen = pen; fill = fill;
    width = w; height = h; ctr = c; contour = s ;
    post_draw = no_drawing; pre_draw = no_drawing }

let hbox ?padding ?pos = align_boxes horizontal ?padding ?pos
let vbox ?padding ?pos = align_boxes vertical ?padding ?pos

let box 
  ?(style=Rect) ?(dx=margin) ?(dy=margin) ?name 
  ?(stroke=Some Color.black) ?pen ?fill b =
  hbox ~style ~dx ~dy ?name ~stroke ?pen ?fill [b]

(* groups the given boxes in a new box *)
let group ?(style=Rect) ?(dx=Num.zero) ?(dy=Num.zero) 
  ?name ?(stroke=None) ?pen ?fill 
  bl =
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
    width = w; height = h; ctr = c; contour = s ;
    post_draw = no_drawing; pre_draw = no_drawing }

let group_array ?name ?stroke ?fill ?dx ?dy ba =
  group ?name ?stroke ?fill ?dx ?dy (Array.to_list ba)

(* groups the given boxes in a rectangular shape of size [w,h]
   and center [c] *)
let group_rect ?name w h c bl =
  { desc = Grp (Array.of_list bl, merge_maps bl);
    name = name; stroke = None; pen = None; fill = None;
    width = w; height = h; ctr = c; 
    contour = Path.shift c (Shapes.rectangle w h);
    post_draw = no_drawing; pre_draw = no_drawing }

let empty ?name ?(width=Num.zero) ?(height=Num.zero) () =
  { desc = Emp; name = name; 
    stroke = None; pen = None; fill = None;
    width = width; height = height; ctr = Point.origin;
    contour = Shapes.rectangle width height ;
    post_draw = no_drawing; pre_draw = no_drawing
  }

type 'a box_creator = 
  ?dx:Num.t -> ?dy:Num.t -> ?name:string -> 
  ?stroke:Color.t option -> ?pen:Pen.t -> ?fill:Color.t -> 'a -> t

let rect = box ~style:Rect
let circle = box ~style:Circle
let ellipse = box ~style:Ellipse
let round_rect = box ~style:RoundRect
let patatoid = box ~style:Patatoid

let tex ?style ?dx ?dy ?name ?(stroke=None) ?pen ?fill s = 
  pic ?style ?dx ?dy ?name ~stroke ?pen ?fill (Picture.tex s)

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

let set_post_draw f b = {b with post_draw = f}
let set_pre_draw f b = {b with pre_draw = f}

let shadow b = 
  let shadow b = 
    let shad i =
      let d = bp (i /. 2.) in
      Command.fill ~color:(Color.gray (0.2 +. i *. 0.2))
        (Path.shift (Point.pt (d, d)) (bpath b)) in
    Command.seq (List.rev_map shad [1. ; 2. ; 3.])
  in
  { b with pre_draw = shadow }

let get_pen b = b.pen
let set_pen p b = { b with pen = Some p }

(* place the box [b] inside the rectangular region defined by upper left
   corner [x,y], width [w] and height [h], according to alignment [pos] *)
let place_box pos x y w h b =
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
  let c = Point.pt (tw_2, Num.neg (th /./ 2.)) in
    (* make a single row with upper left corner [x,y] and height [hrow] *)
  let rec make_row x y hrow wmaxl pl = 
    match pl, wmaxl with
      | [], [] -> []
      | [], _ | _, [] -> assert false
      | p :: ql, wrow :: wl ->
	  let b = place_box pos x y wrow hrow p in
	    b :: make_row (x +/ wrow +/ dx) y hrow wl ql
  in
    (* make all rows, with upper left corner [0,y] *)
  let rec make_array hmaxl y pll = 
    match pll, hmaxl with
      | [], [] -> []
      | [], _ | _, [] -> assert false
      | row :: qll, hrow :: hl -> 
	  let brow = 
	    let c = Point.pt (tw_2, y -/ hrow /./ 2.) in
	      group_rect tw hrow c (make_row Num.zero y hrow wmaxl row)
	  in
	    brow :: make_array hl (y -/ hrow -/ dy) qll
  in
  group_rect tw th c (make_array hmaxl Num.zero pll)

let tabular ?(hpadding=Num.zero) ?(vpadding=Num.zero) ?pos m =
  let pll = Array.to_list (Array.map Array.to_list m) in
  tabularl ~hpadding ~vpadding ?pos pll

let tabulari ?(hpadding=Num.zero) ?(vpadding=Num.zero) ?pos w h f =
  let m = Array.init h (fun j -> Array.init w (fun i -> f i j)) in
  tabular ~hpadding ~vpadding ?pos m

(* blocks *)
let gridl ?(pos=`Center) pll =
  let hmax = Num.fold_max (Num.fold_max height Num.zero) Num.zero pll in
  let wmax = Num.fold_max (Num.fold_max width Num.zero) Num.zero pll in
  let tw = 
    (match pll with [] -> 0. | a::_ -> float (List.length a)) *./ wmax in
  let tw_2 = tw /./ 2. in
  let th = (float (List.length pll)) *./ hmax in
  let c = Point.pt (tw_2, Num.neg (th /./ 2.)) in
    (* make a single row with upper left corner [x,y] *)
  let rec make_row x y = function
    | [] -> []
    | p :: ql -> 
	let b = place_box pos x y wmax hmax p in
	let b' = set_stroke Color.black (group_rect wmax hmax b.ctr [b]) in
	  b' :: make_row (x +/ wmax) y ql
  in
    (* make all rows, with upper left corner [0,y] *)
  let rec make_array y pll =
    match pll with
      | [] -> []
      | row :: qll -> 
	  let brow = 
	    let c = Point.pt (tw_2, y -/ hmax /./ 2.) in
	      group_rect tw hmax c (make_row Num.zero y row)
	  in
	    brow :: make_array (y -/ hmax) qll
  in
  group_rect tw th c (make_array Num.zero pll)

let grid ?pos m =
  let pll = Array.to_list (Array.map Array.to_list m) in 
    gridl ?pos pll

let gridi ?pos w h f =
  let m = Array.init h (fun j -> Array.init w (fun i -> f i j)) in
    grid ?pos m

let hblock ?(pos=`Center) ?name ?(min_width=Num.zero) ?(same_width=false) pl =
  let hmax = Num.fold_max height Num.zero pl in
  let min_width = 
    if same_width then
      let wmax = Num.fold_max width Num.zero pl in
      Num.maxn wmax min_width
    else min_width
  in
  let hmax_2 = hmax /./ 2. in
  let rec make_new acc x = function
    | [] -> 
	List.rev acc, x
    | p :: pl ->
        let wp = width p and hp = height p in
        let wp = Num.maxn min_width wp in
        let y = match pos with
          | `Center -> hmax_2
          | `Top -> hmax -/ hp /./ 2.
          | `Bot -> hp /./ 2.
        in
	let xc = x +/ wp /./ 2. in
        let c = Point.pt (xc, y) in 
        let b = center c p in
        let nc = Point.pt (xc, hmax_2) in
        let b = 
          { b with stroke = Some Color.black; width = wp; height = hmax;
                   ctr = nc; 
                   contour = Path.shift nc (Shapes.rectangle wp hmax)
          } in
        make_new (b::acc) (x +/ wp) pl
  in
  let l,x = make_new [] Num.zero pl in
  let mycenter = Point.pt (x /./ 2., hmax_2) in     
  group_rect ?name x hmax mycenter l

let vblock ?(pos=`Center) ?name ?(min_height=Num.zero) ?(same_height=false) pl =
  let wmax = Num.fold_max width Num.zero pl in
  let min_height =
    if same_height then
      let hmax = Num.fold_max height Num.zero pl in
      Num.maxn hmax min_height
    else min_height
  in
  let wmax_2 = wmax /./ 2. in
  let rec make_new acc y = function
    | [] -> 
	List.rev acc, y
    | p :: pl ->
        let wp = width p and hp = height p in
        let hp = Num.maxn hp min_height in
        let x = match pos with
          | `Center -> wmax_2
          | `Right -> wmax -/ wp /./ 2.
          | `Left ->  wp /./ 2.
        in
	let yc = y -/ hp /./ 2. in
        let c = Point.pt (x, yc) in 
        let b = center c p in
        let nc = Point.pt (wmax_2, yc) in
        let b = 
          { b with stroke = Some Color.black; width = wmax; height = hp;
                   ctr = nc; 
                   contour = Path.shift nc (Shapes.rectangle wmax hp)
          } in
        make_new (b::acc) (y -/ hp) pl
  in
  let l,y = make_new [] Num.zero pl in
  let mycenter = Point.pt (wmax_2, y /./ 2.) in
  group_rect ?name wmax (Num.neg y) mycenter l

open Path

let cpath ?style ?outd ?ind a b =
  let r,l = outd, ind in
  let p = pathk ?style [knotp ?r (ctr a); knotp ?l (ctr b)] in
  cut_after (bpath b) (cut_before (bpath a) p)

let thick_arrow ?style ?(boxed=true) ?line_color ?fill_color ?outd ?ind ?width
    ?head_length ?head_width a b =
  let p = cpath a b in
  let pa = Path.point 0. p in
  let pb = Path.point 1. p in
  Arrow.draw_thick ?style ~boxed ?line_color ?fill_color ?outd ?ind ?width
    ?head_length ?head_width pa pb

