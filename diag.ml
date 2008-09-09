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

open Helpers

module Node = struct

  type t = { 
    box_style : (Point.t -> Picture.t -> Box.t) option;
    id : int; 
    fill : Color.t option;
    x : float; 
    y : float; 
    s : Picture.t; }

  let create = 
    let c = ref min_int in 
    fun ?style fill x y s -> 
      incr c; { box_style = style; id = !c; fill = fill; x = x; y = y; s = s; }

  let hash n = 
    n.id

  let equal n1 n2 =
    n1.id == n2.id

end

module Hnode = Hashtbl.Make(Node)

open Node

type node = Node.t

let node ?style ?fill x y s = Node.create ?style fill x y (Picture.tex s)
let pic_node ?style ?fill = Node.create ?style fill

type dir = Up | Down | Left | Right | Angle of float

type arrow = { 
  src : node; 
  dst : node; 
  lab : string; 
  line_width : Num.t option;
  head : bool;
  dashed : Types.dash option;
  pos : Command.position option;
  outd : dir option;
  ind : dir option;
}

type t = {
  nodes : node list;
  boxes : Box.t Hnode.t;
  mutable arrows: arrow list;
}

let create l = 
  { nodes = l; boxes = Hnode.create 17; arrows = [] }

let arrow d ?(lab="") ?line_width ?pos ?(head=true) ?dashed ?outd ?ind n1 n2 =
  d.arrows <- 
    { src = n1; dst = n2; lab = lab; line_width = line_width ;
      head = head;  dashed = dashed;
      pos = pos; outd = outd; ind = ind } 
  :: d.arrows

let outdir = function
  | Up -> Path.Vec Point.up
  | Down -> Path.Vec Point.down
  | Left -> Path.Vec Point.left
  | Right -> Path.Vec Point.right
  | Angle f -> Path.Vec (Point.dir f)

let indir = function
  | Up -> Path.Vec Point.down
  | Down -> Path.Vec Point.up
  | Left -> Path.Vec Point.right
  | Right -> Path.Vec Point.left
  | Angle f -> Path.Vec (Point.dir f)

let outdir = function None -> None | Some x -> Some (outdir x)
let indir = function None -> None | Some x -> Some (indir x)

type node_style = Point.t -> Picture.t -> Box.t

let make_box ~style ~scale d n = 
  let p = Point.pt (scale n.x, scale n.y) in
  let pic = n.s in
  let b = match n.box_style with 
  | None -> style p pic
  | Some f -> f p pic
  in
  Hnode.add d.boxes n b;
  b

let box_of d = Hnode.find d.boxes

let draw_arrow ?stroke ?pen ?dashed d a =
  let src = box_of d a.src in
  let dst = box_of d a.dst in
  match a.line_width with
    | None ->
	let ba, bla = 
	  if a.head then box_arrow, box_label_arrow
	  else box_line, box_label_line in
	if a.lab = "" then 
	  ba ?color:stroke ?pen ?dashed:a.dashed 
	    ?outd:(outdir a.outd) ?ind:(indir a.ind) src dst
	else
	  bla 
	    ?color:stroke ?pen ?dashed:a.dashed 
	    ?outd:(outdir a.outd) ?ind:(indir a.ind) 
	    ?pos:a.pos (Picture.tex a.lab) src dst
    | Some width ->
	let path = Box.cpath ?outd:(outdir a.outd) ?ind:(indir a.ind) src dst in
	let src = Path.point 0. path in
	let dst = Path.point 1. path in
	Arrow.draw_thick ?outd:(outdir a.outd) ?ind:(indir a.ind) 
	    ~width src dst

let fortybp x = Num.bp (40. *. x)

let defaultbox = Box.round_rect ~dx:Num.two ~dy:Num.two

let draw ?(scale=fortybp) ?(style=defaultbox) 
    ?boxed ?fill ?stroke ?pen d =
  let l = 
    List.map 
      (fun n -> 
	 let fill = if n.fill <> None then n.fill else fill in
	   Box.draw ?fill ?boxed (make_box ~style ~scale d n)) d.nodes
  in
  Command.seq (l @ List.map (draw_arrow ?stroke ?pen d) d.arrows)


