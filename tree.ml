(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Johannes Kanig, Stephane Lescuyer                       *)
(*  and Jean-Christophe Filliatre                                         *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2, with the special exception on linking              *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

open Command
open Helpers

type node_style = Circle | Rect

type t = N of node_style option * Color.t option * string * t list

let leaf ?style ?fill s = N (style, fill, s, [])
let node ?style ?fill s l = N (style, fill, s, l)
let bin  ?style ?fill s x y = N (style, fill, s, [x; y])

let rec width cs = function
  | [] -> 0.
  | [w, _] -> w
  | (w, _) :: l -> w +. cs +. width cs l

let box style p pic = match style with
  | Circle -> Box.circle p pic
  | Rect -> Box.rect p pic

type arrow_style = Directed | Undirected

let arc style ?stroke ?pen b1 b2 = match style with
  | Directed -> box_arrow ?color:stroke ?pen b1 b2
  | Undirected -> box_line ?color:stroke ?pen b1 b2

let draw ?(scale=Num.cm) ?(node_style=Circle) ?(arrow_style=Directed)
  ?fill ?stroke ?pen
  ?(ls=1.0) ?(nw=0.5) ?(cs=0.2) t =
  let point x y = Point.p (scale x, scale y) in
  (* tree -> float * (float -> float -> box * figure) *)
  let rec draw (N (nstyle, nfill, s, l)) =
    let l = List.map draw l in
    let w = max nw (width cs l) in
    w,
    fun x y -> 
      let node_style = match nstyle with None -> node_style | Some s -> s in
      let fill = match nfill with None -> fill | Some _ -> nfill in
      let b = box node_style (point x y) (Picture.tex s) in
      let x = ref (x -. w /. 2.) in
      b, 
      draw_box ?fill b :: 
	List.map 
	(fun (wc,fc) -> 
	   let b',fig = fc (!x +. wc /. 2.) (y -. ls) in
	   x := !x +. wc +. cs;
	   append (seq fig) (arc ?stroke ?pen arrow_style b b')
	) l
  in
  let _,f = draw t in
  snd (f 0. 0.)


  
