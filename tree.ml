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

let my_fold_left f x a =
  (* iterate starting from index 1 *)
  let r = ref x in
  for i = 1 to Array.length a - 1 do
    r := f !r (Array.unsafe_get a i)
  done;
  !r

let arc_wrap ?stroke ?pen arrow_style edge_style b1 b2 =
  let arc x = arc ?stroke ?pen arrow_style edge_style b1 x in
  let a = Box.elts b2 in
  match Array.length a with
  | 0 -> arc b2
  | _ -> arc a.(0)

let draw 
    ?(arrow_style=Directed) ?(edge_style=Straight)
    ?stroke ?pen ?debug  t =
  let rec draw t =
    let a = Box.elts t in
    match Array.length a with
    | 0 -> nop
    | 1 -> Box.draw ?debug a.(0)
    | _ -> 
        seq 
        [Box.draw ?debug a.(0) ; 
         seq (my_fold_left (fun acc x -> draw x :: acc) [] a);
         seq (my_fold_left 
           (fun acc x -> 
             (arc_wrap ?stroke ?pen arrow_style edge_style a.(0) x) :: acc) 
           [] a) 
        ]
  in
  draw t


let leaf s = Box.group [s]
let node ?(ls=Num.bp 12.) ?(cs=Num.bp 5.) s l = 
  let l = Box.hbox ~padding:cs ~pos:`Top l in 
  let n = Box.vbox ~padding:ls [s;l] in
  let s = Box.nth 0 n in
  let l = Box.elts_list (Box.nth 1 n) in
  Box.group (s::l)

let bin ?ls ?cs s x y = node ?ls ?cs s [x;y]

(*
let rec set_fill c (N (n,l)) =
  N (Box.set_fill c n, List.map (set_fill c) l)
*)
