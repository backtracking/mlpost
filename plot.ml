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
open Path

type grid = 
    {
      width : int;
      height : int;
      stepx : Num.t;
      stepy : Num.t;
    }

let mk_grid width height stepx stepy =
  {width = width; height = height; stepx = stepx; stepy = stepy}

type orientation = 
  | Horizontal | Vertical | Both

type labels = 
  | NoLabels
  | Label of (int -> Picture.t) * position

type ticks =
  | NoTicks
  | Ticks of Num.t * Pen.t

type grid_option =
  | Pattern of orientation * (int -> Dash.t)
  | Style of orientation * (int -> Pen.t)
  | Axis of int * orientation * Pen.t * ticks * labels
  | Box of Pen.t

type parsed_grid_options =
    { hpattern : (int -> Dash.t) option;
      vpattern : (int -> Dash.t) option;
      hstyle : (int -> Pen.t) option;
      vstyle : (int -> Pen.t) option;
      axis : int -> orientation -> (Pen.t * ticks * labels) option;
      box : Pen.t option;
    }      

let parse_options =
  let parse_opt acc = function
    | Pattern (Both, dash) -> 
	{acc with hpattern = Some dash; vpattern = Some dash}
    | Pattern (Horizontal, dash) -> {acc with hpattern = Some dash}
    | Pattern (Vertical, dash) -> {acc with vpattern = Some dash}
    | Style (Both, pen) -> 
	{acc with hstyle = Some pen; vstyle = Some pen}
    | Style (Horizontal, pen) -> {acc with hstyle = Some pen}
    | Style (Vertical, pen) -> {acc with vstyle = Some pen}
    | Axis (pos, o, pen, ticks, labels) -> 
	{acc with axis = fun pos' o' ->
	   if pos = pos' && o = o' then Some (pen, ticks, labels)
	   else acc.axis pos' o'
	}
    | Box pen -> {acc with box = Some pen}
  in
    List.fold_left parse_opt 
      {
	hpattern = None;
	vpattern = None;
	hstyle = None;
	vstyle = None;
	axis = (fun _ _ -> None);
	box = None;
      }

let rec fold_from_to f acc a b =
  if a <= b then fold_from_to f (f acc a) (a+1) b else acc

let draw_grid ?(options=[]) {width=w; height=h; stepx=sx; stepy=sy} =
  let fx,fy = float_of_int w, float_of_int h in
  let maxl, maxr = 0., sx *. fx in
  let maxu, maxd = sy *. fy, 0. in
  let ul, ur = (0., maxu), (maxr, maxu)  in
  let ll, lr = (0., 0.), (maxr, 0.)  in
  let opt = parse_options options in
    
  let linei op os = match op, os with 
    | None, None -> fun i p -> Command.draw p
    | None, Some pen -> fun i p -> Command.draw ~pen:(pen i) p
    | Some pat, None -> fun i p -> Command.draw ~dashed:(pat i) p
    | Some pat, Some pen -> 
	fun i p -> Command.draw ~pen:(pen i) ~dashed:(pat i) p
  in
  let axis i o line p labf tickf =
    match opt.axis i o with
      | Some(pen, ticks, labels) ->
	  let label =
	    match labels with 
	      | NoLabels -> []
	      | Label (pici, pos) ->
		  labf (fun i pt -> label ~pos (pici i) pt) in
	  let tick =
	    match ticks with
	      | NoTicks -> []
	      | Ticks (size, pen) -> 
		  tickf (fun pt -> Command.draw ~pen (pt size)) in
	    (Command.draw ~pen p)::tick@label
      | None -> [line i p]
  in      
  let horizontal i = 
    let y = float_of_int i *. sy in
    let pi = path [maxl, y; maxr, y] in
    let pt i y = Point.p (maxl +. (float_of_int i) *. sx, y) in
    let label f =
      fold_from_to (fun l i ->  (f i (pt i y))::l) [] 0 w in
    let tick f =
      let pth i size = pathp [pt i y; pt i (y +. size)] in
	fold_from_to (fun l i -> (f (pth i))::l) [] 0 w in
      axis i Horizontal (linei opt.hpattern opt.hstyle) pi label tick
  in
  let vertical i = 
    let x = float_of_int i *. sx in
    let pi = path [x, maxd; x, maxu] in
    let pt i x = Point.p (x, maxd +. (float_of_int i) *. sy) in
    let label f =
      fold_from_to (fun l i ->  (f i (pt i x))::l) [] 0 h in
    let tick f =
      let pth i size = pathp [pt i x; pt i (x +. size)] in
	fold_from_to (fun l i -> (f (pth i))::l) [] 0 h in
      axis i Vertical (linei opt.vpattern opt.vstyle) pi label tick
  in
  let box = match opt.box with 
    | Some pen -> 
	[Command.draw ~pen (path ~style:JLine ~cycle:JLine [ll; lr; ur; ul])]
    | None -> []
  in
    seq (fold_from_to
	   (fun acc i -> (seq (horizontal i)) :: acc)
	   (fold_from_to
              (fun acc i -> (seq (vertical i)) :: acc) 
	      box 0 w) 0 h)


(* let draw_grid ?(options=[]) {width=w; height=h; stepx=sx; stepy=sy} = *)
(*   let fx,fy = float_of_int w, float_of_int h in *)
(*   let maxl, maxr = 0., sx *. fx in *)
(*   let maxu, maxd = sy *. fy, 0. in *)
(*   let ul, ur = (0., maxu), (maxr, maxu)  in *)
(*   let ll, lr = (0., 0.), (maxr, 0.)  in *)
(*   let box = path ~style:JLine ~cycle:JLine [ll; lr; ur; ul] in *)
(*   let pattern = Dash.scaled 0.5 Dash.withdots in *)
(*   let horizontal i =  *)
(*     let y = float_of_int i *. sy in *)
(*     [Command.draw ~dashed:pattern (path [maxl, y; maxr, y]); *)
(*      Command.draw (path [maxl, y; maxl +. sx /. 3., y]); *)
(*      Command.draw (path [maxr, y; maxr -. sx /. 3., y]); *)
(*      label ~pos:Pleft (Picture.tex (string_of_int i)) (Point.p (maxl, y)); *)
(*     ] *)
(*   in *)
(*   let vertical i =  *)
(*     let x = float_of_int i *. sx in *)
(*     [Command.draw ~dashed:pattern (path [x, maxd; x, maxu]); *)
(*      Command.draw (path [x, maxl; x, maxd +. sy /. 3.]); *)
(*      Command.draw (path [x, maxu; x, maxu -. sy /. 3.]); *)
(*      label ~pos:Pbot (Picture.tex (string_of_int i)) (Point.p (x, maxd)); *)
(*     ] *)
(*   in *)
(*     seq (fold_from_to *)
(* 	   (fun acc i -> (seq (horizontal i)) :: acc) *)
(* 	   (fold_from_to *)
(*               (fun acc i -> (seq (vertical i)) :: acc)  *)
(*               [Command.draw box] 0 w) 0 h) *)
