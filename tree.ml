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

type node_style = Circle | Rect

module Node = struct
  type t = { box : Box.t; fill : Color.t option }
  type repr = t
  let v n = n
  let width n = Box.width n.box
  let height n = Box.height n.box
  let ctr n = Box.ctr n.box
  let shift p n = { n with box = Box.shift p n.box }
end
open Node

type t = Node.t Pos.tree 

let mk_node fill style pic = match style with
  | Circle -> { fill = fill; box = Box.circle Point.origin pic }
  | Rect -> { fill = fill; box = Box.rect Point.origin pic }

let leaf ?(style=Circle) ?fill s = N (mk_node fill style (Picture.tex s), [])
let node ?(style=Circle) ?fill s l = N (mk_node fill style (Picture.tex s), l)
let bin  ?(style=Circle) ?fill s x y = 
  N (mk_node fill style (Picture.tex s), [x; y])

module Pic = struct
  let leaf ?(style=Circle) ?fill s = N (mk_node fill style s, [])
  let node ?(style=Circle) ?fill s l = N (mk_node fill style s, l)
  let bin  ?(style=Circle) ?fill s x y = N (mk_node fill style s, [x; y])
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
      | Straight -> boxdraw ~style:JLine b1 b2
      | Curve -> 
	  let p1, p2 = Box.center b1, Box.center b2 in
	  let corner = Point.pt (x2-/(x2-/x1)//(f 4.),(y1+/y2)//(f 2.)) in
	  let p = pathk ~style:JCurve
	    [NoDir, p1, Vec (Point.sub corner p1); 
	     NoDir, corner, NoDir; 
	     Vec (Point.sub p2 corner), p2, NoDir] in
	  let parrow = 
	    cut_after (Box.bpath b2) (cut_before (Box.bpath b1) p)
	  in
	    linedraw parrow
      | Square -> 
	  let corner = Point.pt (x2,y1) in
	  let p = pathp ~style:JLine 
	    [Box.center b1; corner; Box.center b2] in
	  let parrow = 
	    cut_after (Box.bpath b2) (cut_before (Box.bpath b1) p) 
	  in
	    linedraw parrow
      | HalfSquare -> 
	  let m = (y1+/y2)//(f 2.) in
	  let corner1, corner2 = Point.pt (x1,m), Point.pt (x2,m) in
	  let p = pathp ~style:JLine 
	    [Box.center b1; corner1; corner2; Box.center b2] in
	  let parrow = 
	    cut_after (Box.bpath b2) (cut_before (Box.bpath b1) p) 
	  in
	    linedraw parrow

module T = Pos.Tree(Node)

let draw 
    ?(arrow_style=Directed) ?(edge_style=Straight)
    ?(boxed=true) ?fill ?stroke ?pen
    ?(ls=f 12.) ?(cs=f 5.) t =
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

(***

let draw ?(scale=Num.cm)
    ?(node_style=Circle) ?(arrow_style=Directed) ?(edge_style=Straight)
    ?(boxed=true) ?fill ?stroke ?pen
    ?(ls=f 1.0) ?(nw=f 0.5) ?(cs=f 0.2) t =
  let point x y = Point.pt (scale 1. */ x, scale 1. */ y) in
  (* tree -> float * (float -> float -> box * figure) *)
  let rec draw (N (nstyle, nfill, s, l)) =
    let l = List.map draw l in
      (** XXX *)
    let w = max nw (width cs l) in
    w,
    fun x y -> 
      let node_style = match nstyle with None -> node_style | Some s -> s in
      let fill = match nfill with None -> fill | Some _ -> nfill in
      let (b,_) as bx = 
	box node_style (point x y) s, (x */ scale 1., y */ scale 1.) in
      let x = ref (x -/ (w // (f 2.))) in
      b, 
      Box.draw ?fill ~boxed b :: 
	List.map 
	(fun (wc,fc) -> 
	   let x',y' = (!x +/ (wc // f 2.)), (y -/ ls) in
	   let b',fig = fc x' y' in
	   let bx' = b', (scale 1. */ x', scale 1. */ y') in
	   x := !x +/ wc +/ cs;
	   Command.append (seq fig) (arc ?stroke ?pen arrow_style edge_style bx bx')
	) l
  in
  let _,f = draw t in
  Command.seq (snd (f (bp 0.) (bp 0.)))

***)
