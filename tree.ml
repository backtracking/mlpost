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

type t = Box.t

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

module T = Pos.Tree(Box)

let is_leaf x = Array.length (Box.elts x ) = 1
let root x = 
  (* if this access is invalid, the box has not been created using 
   * [leaf], [node] or [bin] *)
  Box.nth 0 x

let children x = 
  if is_leaf x then []
  else Box.elts_list (Box.nth 1 x)

let leaf s = Box.group [s]
let node ?(ls=Num.bp 12.) ?(cs=Num.bp 5.) ?(arrow_style=Directed)
         ?(edge_style=Straight) ?stroke ?pen s l = 
  let l = Box.hbox ~padding:cs ~pos:`Top l in 
  let tree = Box.vbox ~padding:ls [s;l] in
  Box.set_draw 
    (Command.iterl 
      (fun child -> arc ?stroke ?pen arrow_style edge_style 
                 (root tree) (root child)) (children tree)
    ) tree

let bin ?ls ?cs ?arrow_style ?edge_style ?stroke ?pen s x y = 
  node ?ls ?cs ?arrow_style ?edge_style ?stroke ?pen s [x;y]
