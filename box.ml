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
  | RoundBox
  | Custom of (Num.t -> Num.t -> Num.t * Num.t * Path.t)

let margin = Num.bp 2.
let fresh_name_prefix = "__anonbox"
let is_anon_name s = 
  let rec aux = function
    | -1 -> true
    | n -> fresh_name_prefix.[n] = s.[n] && aux (n-1) in
  (String.length s > String.length fresh_name_prefix)
    && aux ((String.length fresh_name_prefix) -1)

module Smap = Map.Make(String)

let print_dom fmt m =
  Format.fprintf fmt "@[{";
  Smap.iter (fun k _ -> Format.fprintf fmt "%s;@ " k) m;
  Format.fprintf fmt "}@]"

type t = {
  name : string;
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


let rec transform t b = 
  let tr = Point.transform t in
  let nw = tr (north_west b) and sw = tr (south_west b)
  and se = tr (south_east b) in
  let hvec = Point.sub nw sw and wvec = Point.sub se sw in
  { b with
    ctr = Point.transform t b.ctr;
    height = Num.abs (ypart hvec) +/ Num.abs (ypart wvec);
    width = Num.abs (xpart hvec) +/ Num.abs (xpart wvec);
    contour = Path.transform t b.contour;
    desc = transform_desc t b.desc;
}

and transform_desc t = function
  | Emp -> Emp
  | Pic p -> Pic (Picture.transform t p)
  | Grp (a , m ) ->
      Grp (Array.map (transform t) a, Smap.map (transform t) m)

let scale f p = transform [Transform.scaled f] p
let rotate f p = transform [Transform.rotated f] p
let shift pt p = transform [Transform.shifted pt] p
let yscale n p = transform [Transform.yscaled n] p
let xscale n p = transform [Transform.xscaled n] p

let center pt x = shift (Point.sub pt x.ctr) x


let border pos b = 
  match pos with
  | `Top -> ypart (ctr b) +/ height b /./ 2.
  | `Bot -> ypart (ctr b) -/ height b /./ 2.
  | `Left -> xpart (ctr b) -/ width b /./ 2.
  | `Right -> xpart (ctr b) +/ width b /./ 2.

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
    | Pic pic -> pic
    | Grp (a, _) -> 
	Command.iter 0 (Array.length a - 1) (fun i -> draw ~debug a.(i))
  in
  let debug_cmd = 
    if debug then 
      (* TODO maybe we should better draw the rectangle [w,h] 
         instead of the contour *)
      let rect = Path.shift b.ctr (Shapes.rectangle b.width b.height) in
      Command.seq 
	[Command.draw ~color:Color.red ~dashed:Dash.evenly rect;
         if is_anon_name b.name then Command.nop 
         else
         Command.label ~pos:`Center 
           (Picture.tex ("\\tiny " ^ (Picture.escape_all b.name))) (north_west b)]
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

let round_box_ w h = 
  w, h, Shapes.round_box w h
let patatoid_ w h = 
  let p = Shapes.patatoid w h in
  let pic = Command.draw p in
  Picture.width pic, Picture.height pic, p

let from_style = function
  | Rect -> rect_
  | Circle -> circ_
  | RoundRect -> round_rect_
  | Patatoid -> patatoid_
  | Ellipse -> ellipse_
  | RoundBox -> round_box_
  | Custom f -> f

let make_contour s ?(dx=margin) ?(dy=margin) w h c =
  let w =  w +/ 2. *./ dx and h = h +/ 2. *./ dy in
  let w,h, p = (from_style s) w h in
  w, h, Path.shift c p

let no_drawing _ = Command.nop

let fresh_name = 
  let x = ref 1 in
  let s = fresh_name_prefix in
  (fun () ->  incr x; s ^ (string_of_int !x))

let mkbox ?(style=Rect) ?dx ?dy ?name ?(stroke=Some Color.black) 
          ?pen ?fill ?(pre_draw=no_drawing) ?(post_draw=no_drawing)
          w h c desc =
  let w,h,s = make_contour style ?dx ?dy w h c in
  let name = match name with | None -> fresh_name () | Some s -> s in
  { desc = desc;
    name = name; stroke = stroke; pen = pen; fill = fill;
    width = w; height = h; ctr = c; contour = s;
    post_draw = post_draw; pre_draw = pre_draw }

let pic ?style ?dx ?dy ?name ?stroke ?pen ?fill pic =
  let c = Picture.ctr pic in
  mkbox ?style ?dx ?dy ?name ?stroke ?pen ?fill 
        (Picture.width pic) (Picture.height pic) c (Pic pic)

let merge_maps =
  let add_one m b =
    let m = match b.desc with
      | Emp | Pic _ -> m
      | Grp (_, m') -> Smap.fold Smap.add m' m in
    Smap.add b.name b m in
  List.fold_left add_one Smap.empty

let box ?style ?dx ?dy ?name ?stroke ?pen ?fill b =
  mkbox ?style ?dx ?dy ?name ?stroke ?pen ?fill 
  (width b) (height b) (ctr b) (Grp ([|b|], merge_maps [b] ))

let path ?style ?dx ?dy ?name ?(stroke=None) ?pen ?fill p = 
  pic ?style ?dx ?dy ?name ~stroke ?pen ?fill (Picture.make (Command.draw p))

let empty ?(width=Num.zero) ?(height=Num.zero) ?style ?name ?(stroke=None) 
          ?pen ?fill () =
  mkbox ?style ?name ~dx:zero ~dy:zero ~stroke ?pen ?fill 
    width height Point.origin Emp

let empty_from_box ?style ?name ?(stroke=None) ?pen ?fill box =
  mkbox ?style ?name ~stroke ?pen ?fill 
    (width box) (height box) (ctr box) Emp
    

(* groups the given boxes in a new box *)
let group ?style ?(dx=Num.zero) ?(dy=Num.zero) 
  ?name ?(stroke=None) ?pen ?fill bl =
    let xmin b = xpart (south_west b) in
    let xmax b = xpart (north_east b) in
    let ymin b = ypart (south_west b) in
    let ymax b = ypart (north_east b) in
    match bl with
    | [] -> empty ~width:dx ~height:dy ?style ?name ~stroke ?pen ?fill ()
    | [b] -> box ?style ~dx ~dy ?name ~stroke ?pen ?fill b
    | b::r ->
        let xmin,xmax,ymin,ymax = 
          List.fold_left 
            (fun (xmin',xmax',ymin',ymax') b ->
              (Num.minn xmin' (xmin b),
              Num.maxn xmax' (xmax b),
              Num.minn ymin' (ymin b),
              Num.maxn ymax' (ymax b)))
            (xmin b, xmax b, ymin b, ymax b) r
        in
        let w = xmax -/ xmin in
        let h = ymax -/ ymin in
        let c = Point.pt (xmin +/ w /./ 2., ymin +/ h /./ 2.) in
        mkbox ?style ~dx ~dy ?name ~stroke ?pen ?fill w h c 
          (Grp (Array.of_list bl, merge_maps bl))

let group_array ?name ?stroke ?fill ?dx ?dy ba =
  group ?name ?stroke ?fill ?dx ?dy (Array.to_list ba)

(* groups the given boxes in a rectangular shape of size [w,h]
   and center [c] *)
let group_rect ?name ?(stroke=None) w h c bl =
  mkbox ~dx:zero ~dy:zero
    ?name ~stroke w h c (Grp (Array.of_list bl, merge_maps bl))


type 'a box_creator = 
  ?dx:Num.t -> ?dy:Num.t -> ?name:string -> 
  ?stroke:Color.t option -> ?pen:Pen.t -> ?fill:Color.t -> 'a -> t

let rect = box ~style:Rect
let circle = box ~style:Circle
let ellipse = box ~style:Ellipse
let round_rect = box ~style:RoundRect
let patatoid = box ~style:Patatoid
let round_box = box ~style:RoundBox

let tex ?style ?dx ?dy ?name ?(stroke=None) ?pen ?fill s = 
  pic ?style ?dx ?dy ?name ~stroke ?pen ?fill (Picture.tex s)

let nth i b = match b.desc with
  | Grp (a, _ ) -> 
      let n = Array.length a - 1 in
      if i < 0 || i > n then 
	invalid_arg (Format.sprintf "Box.nth: index %d out of 0..%d" i n);
      a.(i)
  | Emp -> invalid_arg "Box.nth: empty box"
  | Pic _ -> invalid_arg "Box.nth: picture box"

let elts b = match b.desc with
  | Emp | Pic _ -> [||]
  | Grp (a, _) -> a

let elts_list b = Array.to_list (elts b)

let get n b = 
  if b.name = n then b else match b.desc with
    | Emp -> invalid_arg "Box.get: empty box"
    | Pic _ -> invalid_arg "Box.get: picture box"
    | Grp (_, m) -> 
	try 
	  Smap.find n m 
	with Not_found -> 
	  invalid_arg 
	    (Misc.sprintf "Box.get: no sub-box %s out of %a" n print_dom m)
            
let sub b1 b2 = get b1.name b2

let get_fill b = b.fill
let set_fill c b = { b with fill = Some c } 

let get_stroke b = b.stroke
let set_stroke s b = {b with stroke = Some s } 
let clear_stroke b = { b with stroke = None }
let get_name b = b.name
let set_name name b = {b with name = name}

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

let set_contour c b = { b with contour = c }

(* new box primitives *)

let ycoord pos a = 
  (* get the vertical position of a box, using a either Top, Bot or the
     center as reference *)
  match pos with
  | `Center -> ypart (ctr a)
  | (`Top | `Bot) as x -> border x a
let xcoord pos a = 
  (* get the horizontal position of a box, using a either Left, Right or the
     center as reference *)
  match pos with
  | `Center -> xpart (ctr a)
  | (`Left | `Right) as x -> border x a

let box_fold f acc l = 
  let _, l = 
  List.fold_left 
    (fun (acc,l) b -> let acc, b = f acc b in acc, b::l) (acc,[]) l in
  List.rev l

let halign ?(pos=`Center) y l = 
  List.map (fun b -> shift (Point.pt (zero, y -/ ycoord pos b)) b) l

let set_height pos h b = 
  let nc = 
    match pos with
    | `Center -> ypart b.ctr
    | `Top -> ypart b.ctr +/ (b.height -/ h) /./ 2.
    | `Bot -> ypart b.ctr -/ (b.height -/ h) /./ 2. in
  { b with height = h; ctr = Point.pt (xpart b.ctr, nc) }

let set_width pos w b = 
  let nc = 
    match pos with
    | `Center -> xpart b.ctr
    | `Left -> xpart b.ctr -/ (b.width -/ w) /./ 2.
    | `Right -> xpart b.ctr +/ (b.width -/ w) /./ 2. in
  { b with width = w; ctr = Point.pt (nc, ypart b.ctr) }

let valign ?(pos=`Center) x l = 
  List.map (fun b -> shift (Point.pt (x -/ xcoord pos b, zero)) b) l

let extractv pos = 
  match pos with
  | `Upleft | `Top | `Upright -> `Top
  | `Left | `Center | `Right -> `Center
  | `Lowleft | `Bot | `Lowright -> `Bot

let extracth pos = 
  match pos with
  | `Upleft | `Left | `Lowleft -> `Left
  | `Top | `Center | `Bot -> `Center
  | `Upright | `Right | `Lowright -> `Right

let set_size pos ~width ~height b = 
  set_height (extractv pos) height (set_width (extracth pos) width b)


let max_height l = Num.fold_max height Num.zero l 
let max_width l = Num.fold_max width Num.zero l 

let same_size ?(pos=`Center) bl = 
  List.map (set_size pos ~width:(max_width bl) ~height:(max_height bl)) bl

let same_height ?(pos=`Center) bl = List.map (set_height pos (max_height bl)) bl
let same_width ?(pos=`Center) bl = List.map (set_width pos (max_width bl)) bl

let hplace ?(padding=zero) ?(pos=`Center) 
           ?(min_width=zero) ?(same_width=false) l =
  if l = [] then [] else
  let min_width =
    if same_width then Num.maxn (max_width l) min_width else min_width in
  let l = 
    List.map 
      (fun b -> set_width (extracth pos) (Num.maxn min_width b.width) b) l in
  let refb = List.hd l in
  let refc = ctr refb and refw = width refb in
  box_fold
    (fun x p -> 
      x+/ p.width +/ padding, 
      center (Point.pt (x +/ p.width /./ 2., ypart p.ctr)) p)
    (xpart refc -/ refw /./ 2.) l

let vplace ?(padding=zero) ?(pos=`Center) 
           ?(min_height=zero) ?(same_height=false) l =
  if l = [] then [] else
  let min_height =
    if same_height then Num.maxn (max_height l) min_height else min_height in
  let l = 
    List.map (fun b ->
      set_height (extractv pos) (Num.maxn min_height b.height) b) l in
  let refb = List.hd l in
  let refc = ctr refb and refh = height refb in
  box_fold
    (fun y p ->
      y -/ p.height -/ padding,
      center (Point.pt (xpart p.ctr, y -/ p.height /./ 2.)) p)
    (ypart refc +/ refh /./ 2.) l

let hbox' ?padding ?(pos=`Center) ?min_width ?same_width l =
  match l with
  | [] -> []
  | _ ->
      let y = ypart (corner pos (List.hd l)) in
      halign ~pos:(extractv pos) y
        (hplace ?padding ~pos:(extracth pos) ?min_width ?same_width l)

let vbox' ?padding ?(pos=`Center) ?min_height ?same_height l =
  match l with
  | [] -> []
  | _ ->
      let x = xpart (corner pos (List.hd l)) in
      valign ~pos:(extracth pos) x
        (vplace ?padding ~pos:(extractv pos) ?min_height ?same_height l)

let hequalize h l = List.map (set_height h) l
let wequalize w l = List.map (set_width w) l

let hbox ?padding ?pos ?style ?dx ?dy ?name ?stroke ?pen ?fill l = 
  group ?style ?dx ?dy ?name ?stroke ?pen ?fill (hbox' ?padding ?pos l)

let vbox ?padding ?pos ?style ?dx ?dy ?name ?stroke ?pen ?fill l = 
  group ?style ?dx ?dy ?name ?stroke ?pen ?fill (vbox' ?padding ?pos l)

let modify_box ?stroke ?pen b = 
  let s = 
    match stroke with
    | None -> Some Color.black
    | Some x -> x in
  { b with stroke = s;
           pen = pen;
           contour = Path.shift b.ctr (Shapes.rectangle b.width b.height) }

let hblock ?(pos=`Center) ?name ?min_width ?same_width pl = 
  group ?name 
    (List.map modify_box (hbox' ~pos ?min_width ?same_width
      (List.map (set_height (extractv pos) (max_height pl)) pl)))

let vblock ?(pos=`Center) ?name ?min_height ?same_height pl =
  group ?name 
    (List.map modify_box 
      (vbox' ~pos ?min_height ?same_height
        (List.map (set_width (extracth pos) (max_width pl)) pl)))

let tabularl ?hpadding ?vpadding ?(pos=`Center) pll =
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
  (* adapt the width of each column *)
  let pll = 
    List.map (fun r-> 
      List.map2 (fun cell w -> set_width (extracth pos) w cell) r wmaxl) pll in
  (* adapt the height of each row *)
  let pll = 
    List.map2 (fun h l -> 
      List.map (fun cell -> set_height (extractv pos) h cell) l) 
      hmaxl pll in
  vbox ~pos ?padding:vpadding 
    (List.map 
      (fun r -> hbox ?padding:hpadding ~pos r) pll)

let tabular ?(hpadding=Num.zero) ?(vpadding=Num.zero) ?pos m =
  let pll = Array.to_list (Array.map Array.to_list m) in
  tabularl ~hpadding ~vpadding ?pos pll

let tabulari ?(hpadding=Num.zero) ?(vpadding=Num.zero) ?pos w h f =
  let m = Array.init h (fun j -> Array.init w (fun i -> f i j)) in
  tabular ~hpadding ~vpadding ?pos m

let gridl ?hpadding ?vpadding ?(pos=`Center) ?stroke ?pen pll =
  let hmax = Num.fold_max (Num.fold_max height Num.zero) Num.zero pll in
  let wmax = Num.fold_max (Num.fold_max width Num.zero) Num.zero pll in
  let pll = 
    List.map (fun l ->
      List.map (fun c -> 
        set_height (extractv pos) hmax
          (set_width (extracth pos) wmax c)) l) pll in
  let pll = 
    vbox' ~pos ?padding:vpadding 
      (List.map 
        (fun r -> 
          group (List.map (modify_box ?stroke ?pen) 
                  (hbox' ?padding:hpadding ~pos r)))
        pll) in
  group pll

let grid ?hpadding ?vpadding ?pos ?stroke ?pen m =
  let pll = Array.to_list (Array.map Array.to_list m) in 
  gridl ?hpadding ?vpadding ?pos ?stroke ?pen pll

let gridi ?hpadding ?vpadding ?pos ?stroke ?pen w h f =
  let m = Array.init h (fun j -> Array.init w (fun i -> f i j)) in
  grid ?hpadding ?vpadding ?pos ?stroke ?pen m
open Path

let strip ?sep p = match sep with
  | None -> p
  | Some n -> Path.strip n p

let cpath ?style ?outd ?ind ?sep a b =
  let r,l = outd, ind in
  let p = pathk ?style [knotp ?r (ctr a); knotp ?l (ctr b)] in
  strip ?sep (cut_after (bpath b) (cut_before (bpath a) p))

let cpath_left ?style ?outd ?ind a b =
  let r,l = outd, ind in
  let p = pathk ?style [knotp ?r (ctr a); knotp ?l b] in
  cut_before (bpath a) p

let cpath_right ?style ?outd ?ind a b =
  let r,l = outd, ind in
  let p = pathk ?style [knotp ?r a; knotp ?l (ctr b)] in
  cut_after (bpath b) p

(* (* Deleted because of circular dependency with the Arrow module.
It did not seem to be used anyway. *)
let thick_arrow ?style ?(boxed=true) ?line_color ?fill_color ?outd ?ind ?width
    ?head_length ?head_width a b =
  let p = cpath a b in
  let pa = Path.point 0. p in
  let pb = Path.point 1. p in
  Arrow.draw_thick ?style ~boxed ?line_color ?fill_color ?outd ?ind ?width
    ?head_length ?head_width pa pb
*)
