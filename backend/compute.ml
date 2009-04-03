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

let memoize f nb =
  let memoize = Hashtbl.create nb in
  fun arg -> 
    try
      Hashtbl.find memoize arg.tag
    with
        Not_found -> 
          let result = f arg.node in
          Hashtbl.add memoize arg.tag result;
          result

let option_compile f = function
  | None -> None, nop
  | Some obj -> 
      let obj, c = f obj in
        Some obj, c

let middle x y = (x/.2.)+.(y/.2.)

let point_of_position (xmin,ymin,xmax,ymax) = function
  | `Top -> {P.x=middle xmin xmax;
          y=ymax}
  | `Bottom -> {P.x=middle xmin xmax;
             y=ymin}
  | `Left -> {P.x=xmax;
          y=middle ymin ymax}
  | `Right -> {P.x=xmin;
          y=middle ymin ymax}
  | `Upleft -> {P.x=xmax;y=ymax}
  | `Upright -> {P.x=xmin;y=ymax}
  | `Lowleft -> {P.x=xmax;y=ymin}
  | `Lowright -> {P.x=xmin;y=ymin}

let rec num' = function
  | F f -> C.F f, nop
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
and num = memoize num' 50
and point' = function
  | PTPair (f1,f2) -> 
      let f1 = num f1 in
      let f2 = num f2 in 
      {P.x=f1;y=f2}
  | PTPointOf (f,p) -> 
      let f = num f in
      let p = path p in
      Spline_lib.point_of_abscisse p f
  | PTDirectionOf (f,p) -> 
      let f = num f in
      let p = path p in
      Spline_lib.direction_of_abscisse p f
  | PTAdd (p1,p2) -> 
      let p1,c1 = point p1 in
      let p2,c2 = point p2 in
        C.PTAdd (p1,p2)
  | PTSub (p1,p2) ->
      let p1 = point p1 in
      let p2 = point p2 in
        Point.add p1 p2
  | PTMult (f,p) ->
      let f = num f in
      let p1 = point p in
        Point.mult p1 f
  | PTRotated (f,p) ->
      let p1 = point p in
      Point.rotated p1 f
  | PTPicCorner (pic, corner) ->
      let p = picture pic in
      let (xmin,ymin,ymax,ymax) = Picture_lib.bounding_box p in
      begin
        match corner with
          | N -> {P.x=middle xmin xmax;
                  y=ymin}
          | S -> {P.x=middle xmin xmax;
                  y=ymax}
          | W -> {P.x=xmin;
                  y=middle ymin ymax}
          | E -> {P.x=xmax;
                  y=middle ymin ymax}
          | NW -> {P.x=xmin;y=ymin}
          | NE -> {P.x=xmax;y=ymin}
          | SW -> {P.x=xmin;y=ymax}
          | SE -> {P.x=xmax;y=ymax}
      end
  | PTTransformed (p,tr) ->
      let p = point p in
      let tr = transform tr in
      Point.transform p tr
and point = memoize point' 50
and knot k =
  match k.Hashcons.node with
    | { knot_in = d1 ; knot_p = p ; knot_out = d2 } ->
        let d1 = direction d1 in
        let p = point p in
        let d2 = direction d2 in
        Spline_lib.Metapath.knot d1 p d2

and joint j = 
  match j.Hashcons.node with
  | JLine -> Spline_lib.Metapath.line
  | JCurve -> Spline_lib.Metapath.curve
  | JCurveNoInflex -> Spline_lib.Metapath.curvenoinflex
  | JTension (a,b) -> Spline_lib.Metapath.tension a b
  | JControls (p1,p2) ->
      let p1 = point p1 in
      let p2 = point p2 in
      Spline_lib.Metapath.join_controls p1 p2
and direction d = 
  match d.Hashcons.node with
  | Vec p -> 
      let p = point p in
      Spline_lib.Metapath.dir_vec p
  | Curl f -> Spline_lib.Metapath.dir_curv f
  | NoDir  -> Spline_lib.Metapath.nodir
and metapath' = function
  | MPAConcat (pa,j,p) ->
      let p = metapath p in
      let pa = knot pa in
      let j = joint j in
      Spline_lib.Metafont.concat p j pa
  | MPAAppend (p1,j,p2) ->
      let p1 = metapath p1 in
      let j = joint j in
      let p2 = metapath p2 in
      Spline_lib.append p1 j p2
  | MPAKnot k -> Spline_lib.Metapath.start (knot k)
  | MPAofPA p -> Spline_lib.Metapath.from_path (path p)
and metapath = memoize metapath' 50
and path' = function
  | PAofMPA p -> Spline_lib.Metapath.to_path (metapath p)
  | MPACycle (d,j,p) ->
      let d = direction d in
      let j = joint j in
      let p = metapath p in
      Spline_lib.Metafont.cycle d j p
  | PATransformed (p,tr) ->
      let p = path p in
      let tr = transform tr in
      Spline_lib.transform p tr
  | PACutAfter (p1,p2) ->
      let p1 = path p1 in
      let p2 = path p2 in
      Spline_lib.cut_after p1 p2
  | PACutBefore (p1,p2) ->
      let p1 = path p1 in
      let p2 = path p2 in
      Spline_lib.cut_before p1 p2
  | PABuildCycle pl ->
      let npl = List.map path pl in
      Spline_lib.build_cycle npl
  | PASub (f1, f2, p) ->
      let f1 = num f1 in
      let f2 = num f2 in
      let p = path p in
      Spline_lib.subpath p f1 f2
  | PABBox p ->
      let p = picture p in
      let (xmin,ymin,xmax,ymax) = Picture_lib.bounding_box p in
      Spline_lib.close_line 
        (Spline_lib.line_to [{P.x=xmin;y=ymin};
                             {P.x=xmin;y=ymax};
                             {P.x=xmax;y=ymax};
                             {P.x=xmax;y=ymin}])
                          
  | PAUnitSquare -> Spline_lib.Approx.unit_square
  | PAQuarterCircle -> Spline_lib.Approx.quarter_circle
  | PAHalfCircle -> Spline_lib.Approx.half_circle
  | PAFullCircle -> Spline_lib.Approx.full_circle
and path = memoize path' 50
and picture' = function
  | PITransformed (p,tr) ->
      let tr = transform tr in
      let pic = picture p in
      Picture_lib.transform pic tr
  | PITex s -> Picture_lib.tex s
  | PIMake c -> command c
  | PIClip (pic,pth) ->
      let pic = picture pic in
      let pth = path pth in
      Picture_lib.clip pic path

and picture pic = memoize picture' 50
and transform t = 
  match t.Hashcons.node with
  | TRRotated f -> Matrix.rotation (num f)
  | TRScaled f -> Matrix.scaled (num f)
  | TRSlanted f -> Matrix.slanted (num f)
  | TRXscaled f -> Matrix.xscaled (num f)
  | TRYscaled f -> Matrix.yscaled (num f)
  | TRShifted p -> Matrix.shifted (point p)
  | TRZscaled p -> Matrix.zscaled (point p)
  | TRReflect (p1,p2) -> Matrix.reflect (point p1) (point p2)
  | TRRotateAround (p,f) -> Matrix.rotate_around (point p) (num f)

and pen p = 
    match p.Hashcons.node with
  | PenCircle -> Picture_lib.PenCircle, Matrix.identity
  | PenSquare -> Picture_lib.PenSquare, Matrix.identity
  | PenFromPath p -> 
      Picture_lib.PenFromPath (path p), Matrix.identity
  | PenTransformed (p, tr) ->
      let p, trp = pen p in
      let tr = Matrix.product (transform tr) trp in
        p, tr

and dash d = 
    match d.Hashcons.node with
  | DEvenly -> Figure_lib.Dash.line [3.;3.]
  | DWithdots -> Figure_lib.Dash.dots [5.]
  | DScaled (f, d) -> 
      let d = dash d in
      Figure_lib.Dash.scale d f
  | DShifted (p,d) ->
      let p = point p in
      let d = dash d in
      Figure_lib.Dash.shifted d p.P.x
  | DPattern l ->
      let l = List.map dash_pattern l in
      Figure_lib.Dash.pattern l

and dash_pattern o = 
    match o.Hashcons.node with
      | On f -> 
	  let f1, c1 = num f in Figure_lib.Dash.On f1
      | Off f -> 
	  let f1, c1 = num f in Figure_lib.Dash.Off f1
	
and command' = function
  | CDraw (p, color, pe, dsh) ->
      let p = path p in
      let pe = (option_compile pen) pe in
      let dsh = (option_compile dash) dsh in
      Figure_lib.draw_path p pe dsh
  | CDrawArrow (p, color, pe, dsh) -> assert false
  | CDrawPic p -> picture p
  | CFill (p, c) -> 
      let p = path p in
      Figure_lib.fill_path p c
  | CSeq l ->
      List.fold_left Figure_lib.on_top (command l)
  | CDotLabel (pic, pos, pt) -> 
      let pic = picture pic in
      let pt = point pt in
      let tr = Matrix.translation
        (Point.sub (point_of_position (Picture_lib.bounding_box pic)) pt) in
      Figure_lib.on_top (Figure_lib.draw_point pt)
        (Figure_lib.transform pic tr)
  | CLabel (pic, pos ,pt) -> 
      let pic = picture pic in
      let pt = point pt in
      let tr = Matrix.translation
        (Point.sub (point_of_position (Picture_lib.bounding_box pic)) pt) in
      Figure_lib.transform pic tr
  | CExternalImage (filename,spec) -> 
      Figure_lib.external_image filename spec
and command = memoize command' 50

let reset () =
  D.NM.clear D.num_map;
  D.NM.clear num_names;
  D.PtM.clear point_names;
  D.PtM.clear D.point_map;
  D.PthM.clear D.path_map;
  D.PthM.clear path_names;
  D.PicM.clear D.picture_map;
  D.PicM.clear picture_names
