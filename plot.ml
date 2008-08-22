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

type ticks = (Num.t * Pen.t) option

let get_style = function
  | None -> fun i -> Dash.evenly, Pen.default ()
  | Some f -> f

let off_pattern = fun i -> Dash.pattern [Dash.On (bp 5.)]
let defpen = fun i -> Pen.default ()

let get_borders sx sy h w = bp 0., sx */ (num_of_int w), 
                            sy */ (num_of_int h), bp 0.

let draw_grid ?(hdash=off_pattern) ?(vdash=off_pattern) 
              ?(hpen=defpen) ?(vpen=defpen) 
              {width=w; height=h; stepx=sx; stepy=sy} =
  let maxl, maxr, maxu, maxd = get_borders sx sy h w in
  let drawline dashed pen p = Command.draw ~pen ~dashed p in
  let horizontal i =
    let y = num_of_int i */ sy in
    let pi = pathn [maxl, y; maxr, y] in
      drawline (hdash i) (hpen i) pi
  in
  let vertical i =
    let x = num_of_int i */ sx in
    let pi = pathn [x, maxd; x, maxu] in
      drawline (vdash i) (vpen i) pi
  in
    seq (Misc.fold_from_to
	   (fun acc i -> (horizontal i) :: acc)
	   (Misc.fold_from_to
              (fun acc i -> (vertical i) :: acc) 
              [] 0 w) 0 h)

let deflabel = Some (fun i -> Picture.tex (string_of_int i)) 
let defticks = Some ((bp 0.25), Pen.default ())

let get_corners maxu maxr = 
  (bp 0., maxu), (maxr, maxu), (bp 0., bp 0.), (maxr, bp 0.)

let draw_axes ?(hpen=Pen.default ()) ?(vpen=Pen.default ()) 
         ?(hlabel= deflabel) ?(vlabel=deflabel)
         ?(ticks=defticks) ?(closed=false) ?hcaption ?vcaption
         {width=w; height=h; stepx=sx; stepy=sy} =
  let maxl, maxr, maxu, maxd = get_borders sx sy h w in
  let ul, ur, ll, lr = get_corners maxu maxr in
  let hcaptcmd = match hcaption with 
    | None -> Command.nop
    | Some labl -> 
	Command.label ~pos:Pleft labl 
	  (Point.pt (num_of_int w */ sx, bp 0. -/sy))
  in
  let vcaptcmd = match vcaption with 
    | None -> Command.nop
    | Some labl -> 
	Command.label ~pos:Plowleft 
	  (Picture.transform [Transform.rotated 90.] labl)
	  (Point.pt (bp 0. -/ sx, num_of_int h */ sy))
  in
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
    let x = num_of_int i */ sx in
      seq [ labelcmd Pbot (Point.pt (x,maxd)) i hlabel; 
            ticks_cmd (fun f -> pathn [x,maxd; x, maxd +/ (sy */ f)]);
            if closed then
              ticks_cmd (fun f -> pathn [x,maxu; x, maxu -/ sy */f])
            else Command.nop ]
  in
  let vertical i =
    let y = num_of_int i */ sy in
      seq [labelcmd Pleft (Point.pt (maxl, y)) i vlabel; 
           ticks_cmd (fun f ->  pathn [maxl,y; maxl +/ sx */ f,y]);
            if closed then
              ticks_cmd (fun f -> pathn [maxr,y; maxr -/ sy */ f, y])
            else Command.nop ]
  in
    seq 
      [Command.draw ~pen:hpen (pathn [ll; lr]);
       Command.draw ~pen:vpen (pathn [ll; ul]);
       if closed then
         seq [Command.draw ~pen:hpen (pathn [ul; ur]);
              Command.draw ~pen:vpen (pathn [lr; ur])]
       else Command.nop;
       hcaptcmd; vcaptcmd;
       seq (Misc.fold_from_to
              (fun acc i -> (horizontal i) :: acc)
              (Misc.fold_from_to
                 (fun acc i -> (vertical i) :: acc) 
               [] 0 h) 0 w) ]

type drawing = | Stepwise | Normal

let draw_func ?(pen) ?(drawing=Normal) ?(style) ?(dashed) ?(label)
    f {width=w; height=h; stepx=sx; stepy=sy} =
  let maxl, maxr, maxu, maxd = get_borders sx sy h w in
  let ul, ur, ll, lr = get_corners maxu maxr in
  let box = pathn ~style:JLine ~cycle:JLine [ul;ll;lr;ur] in
  let normal acc i =
    let x, y = (num_of_int i) */ sx, (Num.bp (f i)) */ sy 
    in 
      (x,y)::acc
  in
  let stepwise (acc,x,y) i =
    let nx, ny = (num_of_int i) */ sx, (Num.bp (f i)) */ sy in
      (nx,ny) :: (nx,y) :: acc, nx, ny
  in
  let graph = 
    match drawing with
    | Normal -> Misc.fold_from_to normal [] 0 w
    | Stepwise -> 
        let p, _,_ = Misc.fold_from_to stepwise ([],Num.bp 0.,Num.bp 0.) 0 w in p
  in
  let pic = Picture.clip 
    (Picture.make (Command.draw ?pen ?dashed (pathn ?style graph))) box in 
    match label with 
      | None -> draw_pic pic 
      | Some (lab, pos, i) -> 
	  let pt = Point.pt (num_of_int i */ sx, (Num.bp (f i)) */ sy) in
	    seq [Command.label ~pos lab pt; draw_pic pic]
