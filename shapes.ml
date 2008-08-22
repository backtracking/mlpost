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

open Command
open Num
open Point
open Path
open Num.Infix
open Types

let pi = 4. *. (atan 1.)
let kappa = F (4. *. (sqrt 2. -. 1.) /. 3.)
let mkappa = F (1. -. (4. *. (sqrt 2. -. 1.) /. 3.))

type borders = 
  { n : Point.t; s : Point.t ; w : Point.t ; e : Point.t; c : Point.t }

type t = { p : path ; b : borders ; ht : Num.t ; wt : Num.t  }

(** Rectangles *)

let build_border w h =
  { n = pt (zero, h) ; s = pt (zero, neg h);
    e = pt (w,zero); w = pt (neg w, zero) ; c = origin }

let rounded_rect_path width height rx ry =
    let hw,hh = width/./ 2.,height/./ 2. in
    let rx = maxn zero (minn rx hw) in
    let ry = maxn zero (minn ry hh) in
      (*     let ul, ur, br, bl =  *)
      (*       pt (-.hw, hh), pt (hw, hh),  *)
      (*       pt (hw, -.hh), pt (-.hw, -.hh) in *)
    let ul1, ul2 = 
      pt (neg hw, hh-/(mkappa*/ry)), pt (mkappa*/rx-/hw, hh) in
    let ur1, ur2 = 
      pt (hw-/mkappa*/rx, hh), pt (hw, hh-/mkappa*/ry) in
    let br1, br2 = 
      pt (hw, mkappa*/ry-/hh), pt (hw-/mkappa*/rx, neg hh) in
    let bl1, bl2 = 
      pt (mkappa*/rx-/hw, neg hh), pt (neg hw, mkappa*/ry-/hh) in
    let knots = 
      [(NoDir, pt (rx-/hw, hh), Vec right);
       (NoDir, pt (hw-/rx, hh), Vec right);
       (NoDir, pt (hw, hh-/ry), Vec down); 
       (NoDir, pt (hw, ry-/hh), Vec down);
       (NoDir, pt (hw-/rx, neg hh), Vec left); 
       (NoDir, pt (rx-/hw, neg hh), Vec left);
       (NoDir, pt (neg hw, ry-/hh), Vec up);
       (NoDir, pt (neg hw, hh-/ry), Vec up)] in
    let joints = 
      [JLine; JControls(ur1,ur2);
       JLine; JControls(br1,br2); 
       JLine; JControls(bl1,bl2); JLine] in
    let path =
      cycle ~dir:(Vec right) ~style:(JControls(ul1,ul2))
	(jointpathk knots joints)
    in
    { p = path; b = build_border hw hh ; wt = width; ht = height }

(** Ellipses and Arcs *)

let full_ellipse_path rx ry =
  let m theta = pt (rx */ (F (cos theta)), ry */ (F (sin theta))) in
  let knots = 
    [(NoDir, m 0., Vec up); (NoDir, m (pi /. 2.), Vec left);
     (NoDir, m pi, Vec down); (NoDir, m (-. pi /. 2.), Vec right)] in
  let r1, r2 = pt (rx, neg (ry*/kappa)), pt (rx, ry*/kappa) in
  let t1, t2 = pt (rx*/kappa , ry), pt (neg (rx*/kappa), ry) in
  let l1, l2 = pt (neg rx, ry*/kappa), pt (neg rx, neg (ry*/kappa)) in
  let b1, b2 = pt (neg (rx*/kappa), neg ry), pt (rx*/kappa, neg ry) in
  let joints =
    [JControls(r2,t1); JControls(t2,l1); JControls(l2,b1)] in
  let path =
    cycle ~dir:(Vec up) 
      ~style:(JControls(b2,r1)) (jointpathk knots joints)
  in 
  { p = path; b = build_border rx ry; wt = 2. *./ rx; ht = 2. *./ ry }
      
(*
let arc_ellipse_path ?(close=false) rx ry theta1 theta2 =
  let curvabs theta =  (2. *. theta) /. pi in
  let path =
    subpath (curvabs theta1) (curvabs theta2) (full_ellipse_path rx ry) in
    if close then
      cycle ~style:JLine (concat ~style:JLine path (NoDir,origin,NoDir))
    else path
*)

let rectangle_path width height =
  let w = width /./ 2. in
  let h = height /./ 2. in
  let mw = neg w in
  let mh = neg h in
  let path = 
    Path.pathn ~cycle:JLine ~style:JLine [ w, mh; w, h; mw, h; mw, mh]
  in
  { p = path; b = build_border w h; wt = width; ht = height }
    

let patatoid width height = 
  let wmin,wmax = -0.5 *./ width, 0.5 *./ width in
  let hmin,hmax = -0.5 *./ height, 0.5 *./ height in
  let ll = pt (wmin,hmin) in
  let lr = pt (wmax,hmin) in
  let ur = pt (wmax,hmax) in
  let ul = pt (wmin, hmax) in
  let a = segment (Random.float 1.) ll lr in
  let b = segment (Random.float 1.) lr ur in
  let c = segment (Random.float 1.) ur ul in
  let d = segment (Random.float 1.) ul ll in
  let p = pathp ~cycle:JCurve [a;b;c;d] in
  let dummypic = Picture.make (Command.draw p) in
   (* corners are wrong *)
  { p = p ; b = build_border wmax hmax;
    wt = Picture.width dummypic; ht = Picture.height dummypic }

let circle d =
  { p= Path.scale d Path.fullcircle;
    b = (let r = 0.5 *./ d in build_border r r); wt = d ; ht = d}
      

let draw_func ?fill ?(stroke=Color.black) ?(thickness=0.5) path =
  let fillcmd =
    match fill with
      | None -> Command.nop
      | Some color -> Command.fill ~color path in
  let pen = Pen.square ~tr:([Transform.scaled (F thickness)]) () in
  let strokecmd = Command.draw ~color:stroke ~pen path in
    Picture.make (Command.seq [fillcmd; strokecmd])

let rounded_rect ?fill ?stroke ?thickness width height rx ry =
  let path = rounded_rect_path width height rx ry in
    draw_func ?fill ?stroke ?thickness path.p
    

let rectangle ?fill ?stroke ?thickness width height =
  let path = rectangle_path width height in
    draw_func ?fill ?stroke ?thickness path.p

let ellipse ?fill ?stroke ?thickness rx ry =
  let path = full_ellipse_path rx ry in
    draw_func ?fill ?stroke ?thickness path.p

let shift_border pt x = 
  { n = Point.shift pt x.n;
    e = Point.shift pt x.e;
    w = Point.shift pt x.w;
    s = Point.shift pt x.s;
    c = Point.shift pt x.c
  }

let shift pt x = 
  { x with p = Path.shift pt x.p;
    b = shift_border pt x.b
  }

let bpath x = x.p
(*
let arc_ellipse ?fill ?stroke ?thickness ?(close=false) rx ry theta1 theta2 =
  let path = arc_ellipse_path rx ry theta1 theta2 
              ~close:(close || (match fill with Some _ -> true | _ -> false))
  in
    draw_func ?fill ?stroke ?thickness path
*)

let north s = s.b.n
let south s = s.b.s
let west  s = s.b.w
let east  s = s.b.e

(* POS compliance *)
type repr = t

let v x = x
let ctr x = x.b.c
let height x = x.ht
let width x = x.wt

let center pt x = shift (Point.sub pt (ctr x)) x
