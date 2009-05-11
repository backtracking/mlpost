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

let default_labeloffset = 2.


open Types
open Hashcons
module P = Point_lib
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

let nop = Picture_lib.empty

let option_compile f = function
  | None -> None
  | Some obj -> Some (f obj)

let middle x y = (x/.2.)+.(y/.2.)

let point_of_position ecart
  ({ P.x = xmin; y = ymin}, { P.x = xmax; y = ymax}) = 
    function
      | `Top -> {P.x=middle xmin xmax; y=ymin-.ecart}
      | `Bot -> {P.x=middle xmin xmax; y=ymax+.ecart}
      | `Left -> {P.x=xmax+.ecart; y=middle ymin ymax}
      | `Right -> {P.x=xmin-.ecart; y=middle ymin ymax}
      | `Upleft -> {P.x=xmax+.ecart;y=ymin-.ecart}
      | `Upright -> {P.x=xmin-.ecart;y=ymin-.ecart}
      | `Lowleft -> {P.x=xmax+.ecart;y=ymax+.ecart}
      | `Lowright -> {P.x=xmin-.ecart;y=ymax+.ecart}
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
      P.add p1 p2
  | PTSub (p1,p2) ->
      let p1 = point p1 in
      let p2 = point p2 in
        P.sub p1 p2
  | PTMult (f,p) ->
      let f = num f in
      let p1 = point p in
        P.mult f p1
  | PTRotated (f,p) ->
      let p1 = point p in
      P.rotated f p1
  | PTPicCorner (pic, corner) ->
      let p = picture pic in
      point_of_position 0. (Picture_lib.bounding_box p) corner
  | PTTransformed (p,tr) ->
      let p = point p in
      let tr = transform tr in
      P.transform tr p
and point p = memoize point' point_memoize p
and knot k =
  match k.Hashcons.node with
    | { knot_in = d1 ; knot_p = p ; knot_out = d2 } ->
        let d1 = direction d1 in
        let p = point p in
        let d2 = direction d2 in
        d1,Spline_lib.Metapath.knot p,d2

and joint dl j dr = 
  match j.Hashcons.node with
  | JLine -> Spline_lib.Metapath.line_joint
  | JCurve -> Spline_lib.Metapath.curve_joint dl dr
  | JCurveNoInflex -> Spline_lib.Metapath.curve_no_inflex_joint dl dr
  | JTension (a,b) -> Spline_lib.Metapath.tension_joint dl a b dr
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
      let pdl,p,pdr = metapath p in
      let dl,pa,dr = knot pa in
      let j = joint pdr j dl in
      pdl,Spline_lib.Metapath.concat p j pa,dr
  | MPAAppend (p1,j,p2) ->
      let p1dl,p1,p1dr = metapath p1 in
      let p2dl,p2,p2dr = metapath p2 in
      let j = joint p1dr j p2dl in
      p1dl,Spline_lib.Metapath.append p1 j p2,p2dr
  | MPAKnot k -> 
      let dl,p,dr = knot k in
      dl,Spline_lib.Metapath.start p, dr
  | MPAofPA p -> 
      Spline_lib.Metapath.no_direction,
      Spline_lib.Metapath.from_path (path p),
      Spline_lib.Metapath.no_direction

and metapath p = memoize metapath' metapath_memoize p
and path' = function
  | PAofMPA p -> 
      let _,mp,_ = (metapath p) in
      Spline_lib.Metapath.to_path mp
  | MPACycle (d,j,p) ->
      let d = direction d in
      let dl,p,_ = metapath p in
      let j = joint d j dl in
      Spline_lib.Metapath.cycle j p
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
      Format.printf "tex : %a@." Gentex.print tex;
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
  | TRShifted p -> 
      let p = point p in
      let m = Matrix.translation p in
      Format.printf "Shifted p:%a  m:%a@." P.print p M.print m;m
  | TRZscaled p -> Matrix.zscaled (point p)
  | TRReflect (p1,p2) -> Matrix.reflect (point p1) (point p2)
  | TRRotateAround (p,f) -> Matrix.rotate_around (point p) f

and dash d = 
    match d.Hashcons.node with
  | DEvenly -> Picture_lib.Dash.line
  | DWithdots -> Picture_lib.Dash.dots
  | DScaled (f, d) -> 
      let d = dash d in
      Picture_lib.Dash.scale f d
  | DShifted (p,d) ->
      let p = point p in
      let d = dash d in
      Picture_lib.Dash.shifted p.P.x d
  | DPattern l ->
      let l = List.map dash_pattern l in
      Picture_lib.Dash.pattern l

and dash_pattern o = 
    match o.Hashcons.node with
      | On f -> Picture_lib.Dash.On (num f)
      | Off f -> Picture_lib.Dash.Off (num f)
	
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
      let mm = (Picture_lib.bounding_box pic) in
      let pos = (point_of_position default_labeloffset mm pos) in
      let tr = Matrix.translation (P.sub pt pos) in
      Picture_lib.on_top (Picture_lib.draw_point pt)
        (Picture_lib.transform tr pic)
  | CLabel (pic, pos ,pt) -> 
      let pic = picture pic in
      let pt = point pt in
      let pos = (point_of_position default_labeloffset (Pi.bounding_box pic) pos) in
      let tr = Matrix.translation (P.sub pt pos) in
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
