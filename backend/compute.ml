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
open Hashcons
module P = Point
module M = Matrix
module S = Spline_lib
module Pi = Picture_lib

let memoize f memoize =
  fun arg -> 
    try
      Hashtbl.find memoize arg.tag
    with
        Not_found -> 
          let result = f arg.node in
          Hashtbl.add memoize arg.tag result;
          result

let nop = Figure_lib.nop

let option_compile f = function
  | None -> None
  | Some obj -> Some (f obj)

let middle x y = (x/.2.)+.(y/.2.)

let point_of_position 
  ({ P.x = xmin; y = ymin}, { P.x = xmax; y = ymax}) = 
    function
      | `Top -> {P.x=middle xmin xmax; y=ymax}
      | `Bot -> {P.x=middle xmin xmax; y=ymin}
      | `Left -> {P.x=xmax; y=middle ymin ymax}
      | `Right -> {P.x=xmin; y=middle ymin ymax}
      | `Upleft -> {P.x=xmax;y=ymax}
      | `Upright -> {P.x=xmin;y=ymax}
      | `Lowleft -> {P.x=xmax;y=ymin}
      | `Lowright -> {P.x=xmin;y=ymin}
      | `Center -> {P.x = middle xmin xmax; P.y = middle ymin ymax }

let num_memoize = Hashtbl.create 50
let point_memoize = Hashtbl.create 50
let metapath_memoize = Hashtbl.create 50
let path_memoize = Hashtbl.create 50
let picture_memoize = Hashtbl.create 50
let command_memoize = Hashtbl.create 50

let prelude = ref ""
let set_prelude = (:=) prelude

let rec num' = function
  | F f -> f
  | NXPart p -> 
      let p = point p in
      p.P.x
  | NYPart p ->
      let p = point p in
      p.P.y
  | NAdd(n1,n2) ->
      let n1 = num n1 in
      let n2 = num n2 in
        n1 +. n2
  | NSub(n1,n2) ->
      let n1 = num n1 in
      let n2 = num n2 in
        n1 -. n2
  | NMult (n1,n2) ->
      let n1 = num n1 in
      let n2 = num n2 in
        n1*.n2
  | NDiv (n1,n2) ->
      let n1 = num n1 in
      let n2 = num n2 in
        n1/.n2
  | NMax (n1,n2) ->
      let n1 = num n1 in
      let n2 = num n2 in
       max n1 n2
  | NMin (n1,n2) ->
      let n1 = num n1 in
      let n2 = num n2 in
        min n1 n2
  | NGMean (n1,n2) ->
      let n1 = num n1 in
      let n2 = num n2 in
        sqrt (n1*.n1+.n2*.n2)
  | NLength p ->
      let p = path p in
      Spline_lib.length p
and num n = memoize num' num_memoize n
and point' = function
  | PTPair (f1,f2) -> 
      let f1 = num f1 in
      let f2 = num f2 in 
      {P.x=f1;y=f2}
  | PTPointOf (f,p) -> 
      let f = num f in
      let p = path p in
      Spline_lib.abscissa_to_point p f
  | PTDirectionOf (f,p) -> 
      let f = num f in
      let p = path p in
      Spline_lib.direction_of_abscissa p f
  | PTAdd (p1,p2) -> 
      let p1 = point p1 in
      let p2 = point p2 in
      Point.add p1 p2
  | PTSub (p1,p2) ->
      let p1 = point p1 in
      let p2 = point p2 in
        Point.sub p1 p2
  | PTMult (f,p) ->
      let f = num f in
      let p1 = point p in
        Point.mult f p1
  | PTRotated (f,p) ->
      let p1 = point p in
      Point.rotated f p1
  | PTPicCorner (pic, corner) ->
      let p = picture pic in
      point_of_position (Picture_lib.bounding_box p) corner
  | PTTransformed (p,tr) ->
      let p = point p in
      let tr = transform tr in
      Point.transform tr p
and point p = memoize point' point_memoize p
and knot k =
  match k.Hashcons.node with
    | { knot_in = d1 ; knot_p = p ; knot_out = d2 } ->
        let d1 = direction d1 in
        let p = point p in
        let d2 = direction d2 in
        Spline_lib.Metapath.knot d1 p d2

and joint j = 
  match j.Hashcons.node with
  | JLine -> Spline_lib.Metapath.line_joint
  | JCurve -> Spline_lib.Metapath.curve_joint
  | JCurveNoInflex -> Spline_lib.Metapath.curve_no_inflex_joint
  | JTension (a,b) -> Spline_lib.Metapath.tension_joint a b
  | JControls (p1,p2) ->
      let p1 = point p1 in
      let p2 = point p2 in
      Spline_lib.Metapath.controls_joint p1 p2
and direction d = 
  match d.Hashcons.node with
  | Vec p -> 
      let p = point p in
      Spline_lib.Metapath.vec_direction p
  | Curl f -> Spline_lib.Metapath.curl_direction f
  | NoDir  -> Spline_lib.Metapath.no_direction
and metapath' = function
  | MPAConcat (pa,j,p) ->
      let p = metapath p in
      let pa = knot pa in
      let j = joint j in
      Spline_lib.Metapath.concat p j pa
  | MPAAppend (p1,j,p2) ->
      let p1 = metapath p1 in
      let j = joint j in
      let p2 = metapath p2 in
      Spline_lib.Metapath.append p1 j p2
  | MPAKnot k -> Spline_lib.Metapath.start (knot k)
  | MPAofPA p -> Spline_lib.Metapath.from_path (path p)
and metapath p = memoize metapath' metapath_memoize p
and path' = function
  | PAofMPA p -> Spline_lib.Metapath.to_path (metapath p)
  | MPACycle (d,j,p) ->
      let d = direction d in
      let j = joint j in
      let p = metapath p in
      Spline_lib.Metapath.cycle d j p
  | PATransformed (p,tr) ->
      let p = path p in
      let tr = transform tr in
      Spline_lib.transform tr p
  | PACutAfter (p1,p2) ->
      let p1 = path p1 in
      let p2 = path p2 in
      Spline_lib.cut_after p1 p2
  | PACutBefore (p1,p2) ->
      let p1 = path p1 in
      let p2 = path p2 in
      Spline_lib.cut_before p1 p2
  | PABuildCycle pl ->
(*       let npl = List.map path pl in *)
      (* TODO *) assert false
(*       Spline_lib.buildcycle npl *)
  | PASub (f1, f2, p) ->
      let f1 = num f1 in
      let f2 = num f2 in
      let p = path p in
      Spline_lib.subpath p f1 f2
  | PABBox p ->
      let p = picture p in
      let pmin,pmax = Picture_lib.bounding_box p in
      Spline_lib.close 
        (Spline_lib.create_lines [{P.x = pmin.P.x; y = pmin.P.y};
                                  {P.x = pmin.P.x; y = pmax.P.y};
                                  {P.x = pmax.P.x; y = pmax.P.y};
                                  {P.x = pmax.P.x; y = pmin.P.y}])
                          
  | PAUnitSquare -> Spline_lib.Approx.unitsquare ()
  | PAQuarterCircle -> Spline_lib.Approx.quartercircle ()
  | PAHalfCircle -> Spline_lib.Approx.halfcirle ()
  | PAFullCircle -> Spline_lib.Approx.fullcircle ()
and path p = memoize path' path_memoize p
and picture' = function
  | PITransformed (p,tr) ->
      let tr = transform tr in
      let pic = picture p in
      Picture_lib.transform tr pic
  | PITex s -> 
      let tex = List.hd (Gentex.create !prelude [s]) in
      Picture_lib.tex tex
  | PIMake c -> command c
  | PIClip (pic,pth) ->
      let pic = picture pic in
      let pth = path pth in
      Picture_lib.clip pic pth

and picture p = memoize picture' picture_memoize p
and transform t = 
  match t.Hashcons.node with
  | TRRotated f -> Matrix.rotation f
  | TRScaled f -> Matrix.scale (num f)
  | TRSlanted f -> Matrix.slanted (num f)
  | TRXscaled f -> Matrix.xscaled (num f)
  | TRYscaled f -> Matrix.yscaled (num f)
  | TRShifted p -> Matrix.translation (point p)
  | TRZscaled p -> Matrix.zscaled (point p)
  | TRReflect (p1,p2) -> Matrix.reflect (point p1) (point p2)
  | TRRotateAround (p,f) -> Matrix.rotate_around (point p) f

and dash d = 
    match d.Hashcons.node with
  | DEvenly -> Figure_lib.Dash.line
  | DWithdots -> Figure_lib.Dash.dots
  | DScaled (f, d) -> 
      let d = dash d in
      Figure_lib.Dash.scale f d
  | DShifted (p,d) ->
      let p = point p in
      let d = dash d in
      Figure_lib.Dash.shifted p.P.x d
  | DPattern l ->
      let l = List.map dash_pattern l in
      Figure_lib.Dash.pattern l

and dash_pattern o = 
    match o.Hashcons.node with
      | On f -> Figure_lib.Dash.On (num f)
      | Off f -> Figure_lib.Dash.Off (num f)
	
and command' = function
  | CDraw (p, c, pe, dsh) ->
      let p = path p in
      let pe = (option_compile pen) pe in
      let dsh = (option_compile dash) dsh in
      Picture_lib.stroke_path p c pe dsh
  | CDrawArrow (p, color, pe, dsh) -> assert false
  | CDrawPic p -> picture p
  | CFill (p, c) -> 
      let p = path p in
      Picture_lib.fill_path p c
  | CSeq l ->
      begin match l with
      | [] -> Picture_lib.empty
      | [x] -> command x
      | (x::r) -> 
          List.fold_left 
          (fun acc c -> Picture_lib.on_top acc (command c)) (command x) r
      end
  | CDotLabel (pic, pos, pt) -> 
      let pic = picture pic in
      let pt = point pt in
      let tr = Matrix.translation
        (Point.sub (point_of_position (Picture_lib.bounding_box pic) pos) pt) in
      Picture_lib.on_top (Picture_lib.draw_point pt)
        (Picture_lib.transform tr pic)
  | CLabel (pic, pos ,pt) -> 
      let pic = picture pic in
      let pt = point pt in
      let tr = Matrix.translation
        (Point.sub (point_of_position (Picture_lib.bounding_box pic) pos) pt) in
      Picture_lib.transform tr pic
  | CExternalImage (filename,sp) -> 
      Picture_lib.external_image filename (spec sp)
and spec = function
  | `Exact (n1,n2) -> `Exact (num n1, num n2)
  | `Height n -> `Height (num n)
  | `Width n -> `Width (num n)
  | `Inside (n1,n2) -> `Inside (num n1, num n2)
  | `None -> `None
and pen p = 
  (* TODO : the bounding box is not aware of the pen size *)
  match p.Hashcons.node with
    | PenCircle -> Matrix.identity
    | PenSquare -> (*TODO not with cairo...*)assert false
        (*Picture_lib.PenSquare*)
    | PenFromPath p -> (*TODO : very hard*)assert false
        (*Picture_lib.PenFromPath (path p)*)
    | PenTransformed (p, tr) ->
        Matrix.multiply (transform tr) (pen p)

and command c = memoize command' command_memoize c
