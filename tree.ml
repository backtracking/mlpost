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
open Helpers
open Path
open Num
open Num.Infix
open Pos

type node_style = ?dx:Num.t -> ?dy:Num.t -> Point.t -> Picture.t -> Box.t

module Node = struct
  type t = { box : Box.t; fill : Color.t option }
  type repr = t
  let v n = n
  let width n = Box.width n.box
  let height n = Box.height n.box
  let ctr n = Box.ctr n.box
  let shift p n = { n with box = Box.shift p n.box }
  let center pt x = shift (Point.sub pt (ctr x)) (v x)
end
open Node

type t = Node.t Pos.tree 

let mk_node fill style pic = 
  { fill = fill; box = style Point.origin pic }

let leaf ?(style=Box.circle) ?fill s = N (mk_node fill style (Picture.tex s), [])
let node ?(style=Box.circle) ?fill s l = N (mk_node fill style (Picture.tex s), l)
let bin  ?(style=Box.circle) ?fill s x y = 
  N (mk_node fill style (Picture.tex s), [x; y])

module Pic = struct
  let leaf ?(style=Box.circle) ?fill s = N (mk_node fill style s, [])
  let node ?(style=Box.circle) ?fill s l = N (mk_node fill style s, l)
  let bin  ?(style=Box.circle) ?fill s x y = N (mk_node fill style s, [x; y])
end

module Bx = struct
  let leaf ?fill b = N ({fill=fill; box=b}, [])
  let node ?fill b l = N ({fill=fill; box=b}, l)
  let bin  ?fill b x y = N ({fill=fill; box=b}, [x; y])
end

type arrow_style = Directed | Undirected

type edge_style = Straight | Curve | Square | HalfSquare

let arc astyle estyle ?stroke ?pen b1 b2 =
  let x1,y1 = let p = Box.ctr b1 in Point.xpart p, Point.ypart p 
  and x2,y2 = let p = Box.ctr b2 in Point.xpart p, Point.ypart p in
  let boxdraw, linedraw  = match astyle with 
    | Directed -> 
	box_arrow ?color:stroke ?pen, draw_arrow ?color:stroke ?pen
    | Undirected -> 
	box_line ?color:stroke ?pen, draw ?color:stroke ?pen 
  in
    match estyle with
      | Straight -> boxdraw ~style:jLine b1 b2
      | Curve -> 
	  let p1, p2 = Box.ctr b1, Box.ctr b2 in
	  let corner = Point.pt (x2-/(x2-/x1) /./ 4.,(y1+/y2) /./ 2.) in
	  let p = pathk ~style:jCurve
	    (knotlist
	       [noDir, p1, vec (Point.sub corner p1); 
		noDir, corner, noDir; 
		vec (Point.sub p2 corner), p2, noDir]) in
	  let parrow = 
	    cut_after (Box.bpath b2) (cut_before (Box.bpath b1) p)
	  in
	    linedraw parrow
      | Square -> 
	  let corner = Point.pt (x2,y1) in
	  let p = pathp ~style:jLine 
	    [Box.ctr b1; corner; Box.ctr b2] in
	  let parrow = 
	    cut_after (Box.bpath b2) (cut_before (Box.bpath b1) p) 
	  in
	    linedraw parrow
      | HalfSquare -> 
	  let m = (y1+/y2) /./ 2. in
	  let corner1, corner2 = Point.pt (x1,m), Point.pt (x2,m) in
	  let p = pathp ~style:jLine 
	    [Box.ctr b1; corner1; corner2; Box.ctr b2] in
	  let parrow = 
	    cut_after (Box.bpath b2) (cut_before (Box.bpath b1) p) 
	  in
	    linedraw parrow

module T = Pos.Tree(Node)

let draw 
    ?(arrow_style=Directed) ?(edge_style=Straight)
    ?(boxed=true) ?fill ?stroke ?pen
    ?(ls=Num.bp 12.) ?(cs=Num.bp 5.) t =
  let t = T.place ~dx:cs ~dy:ls t in
  let rec draw (N (n, l)) = 
    let fill = match n.fill with None -> fill | Some _ -> n.fill in
    seq 
      (Box.draw ?fill ~boxed n.box ::
       iterl draw l ::
       List.map 
         (function (N (n', _)) -> 
	   arc ?stroke ?pen arrow_style edge_style n.box n'.box) l)
  in
  draw (T.v t)
