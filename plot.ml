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

type skeleton = 
    {
      width : int;
      height : int;
      stepx : Num.t;
      stepy : Num.t;
    }

let mk_skeleton width height stepx stepy =
  {width = width; height = height; stepx = stepx; stepy = stepy}

type labels = (int -> Picture.t) option

type ticks = (float * Pen.t) option

let get_style = function
  | None -> fun i -> Dash.evenly, Pen.default ()
  | Some f -> f

let off_pattern = fun i -> Dash.pattern [Dash.On 5.]
let defpen = fun i -> Pen.default ()

let get_borders sx sy h w = 0., sx *. (float_of_int w), 
                            sy *. (float_of_int h), 0.

let draw_grid ?(hdash=off_pattern) ?(vdash=off_pattern) 
              ?(hpen=defpen) ?(vpen=defpen) 
              {width=w; height=h; stepx=sx; stepy=sy} =
  let maxl, maxr, maxu, maxd = get_borders sx sy h w in
  let drawline dashed pen p = Command.draw ~pen ~dashed p in
  let horizontal i =
    let y = float_of_int i *. sy in
    let pi = path [maxl, y; maxr, y] in
      drawline (hdash i) (hpen i) pi
  in
  let vertical i =
    let x = float_of_int i *. sx in
    let pi = path [x, maxd; x, maxu] in
      drawline (vdash i) (vpen i) pi
  in
    seq (Misc.fold_from_to
	   (fun acc i -> (horizontal i) :: acc)
	   (Misc.fold_from_to
              (fun acc i -> (vertical i) :: acc) 
              [] 0 w) 0 h)

let deflabel = Some (fun i -> Picture.tex (string_of_int i)) 
let defticks = Some (0.25, Pen.default ())

let get_corners maxu maxr = (0., maxu), (maxr, maxu), (0., 0.), (maxr, 0.)

let draw_axes ?(hpen=Pen.default ()) ?(vpen=Pen.default ()) 
         ?(hlabel= deflabel ) ?(vlabel=deflabel)
         ?(ticks=defticks) ?(closed=false) 
         {width=w; height=h; stepx=sx; stepy=sy} =
  let maxl, maxr, maxu, maxd = get_borders sx sy h w in
  let ul, ur, ll, lr = get_corners maxu maxr in
  let labelcmd pos p i = function
    | None -> Command.nop
    | Some f -> Command.label ~pos (f i) p
  in
  let ticks_cmd pathf = 
    match ticks with
    | None -> Command.nop
    | Some (f,pen) -> Command.draw ~pen (pathf f)
  in
  let horizontal i =
    let x = float_of_int i *. sx in
      seq [ labelcmd Pbot (Point.p (x,maxd)) i hlabel; 
            ticks_cmd (fun f -> path [x,maxd; x, maxd +. sy *. f]);
            if closed then
              ticks_cmd (fun f -> path [x,maxu; x, maxu -. sy *.f])
            else Command.nop ]
  in
  let vertical i =
    let y = float_of_int i *. sy in
      seq [labelcmd Pleft (Point.p (maxl, y)) i vlabel; 
           ticks_cmd (fun f ->  path [maxl,y; maxl +. sx *. f,y]);
            if closed then
              ticks_cmd (fun f -> path [maxr,y; maxr -. sy *. f, y])
            else Command.nop ]
  in
    seq 
      [Command.draw ~pen:hpen (path [ll; lr]);
       Command.draw ~pen:vpen (path [ll; ul]);
       if closed then
         seq [Command.draw ~pen:hpen (path [ul; ur]);
              Command.draw ~pen:vpen (path [lr; ur])]
       else Command.nop;
       seq (Misc.fold_from_to
              (fun acc i -> (horizontal i) :: acc)
              (Misc.fold_from_to
                 (fun acc i -> (vertical i) :: acc) 
               [] 0 h) 0 w) ]

type drawing = | Stepwise | Normal

let draw_func ?(pen) ?(drawing=Normal) ?(style) f {width=w; height=h; stepx=sx; stepy=sy} =
  let maxl, maxr, maxu, maxd = get_borders sx sy h w in
  let ul, ur, ll, lr = get_corners maxu maxr in
  let box = path ~style:JLine ~cycle:JLine [ul;ll;lr;ur] in
  let normal acc i =
    let x, y = (float_of_int i) *. sx, (f i) *. sy 
    in 
      (x,y)::acc
  in
  let stepwise (acc,x,y) i =
    let nx, ny = (float_of_int i) *. sx, (f i) *. sy in
      (nx,ny) :: (nx,y) :: acc, nx, ny
  in
  let graph = 
    match drawing with
    | Normal -> Misc.fold_from_to normal [] 0 w
    | Stepwise -> 
        let p, _,_ = Misc.fold_from_to stepwise ([],0.,0.) 0 w in
          p
  in
    let pic = Picture.clip 
               (Picture.make (Command.draw ?pen (path ?style graph))) box in 
      draw_pic pic
