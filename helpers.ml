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

open Path
open Point
open Num
open Command

(*  puts labels at given points with given text *)
let dotlabels ?(pos=Pcenter) ls lp =
  List.map2 (fun s p -> dotlabel ~pos:pos (Picture.tex s) p) ls lp

let draw_simple_arrow ?color ?pen ?(style=defaultjoint) ?(outd=NoDir) ?(ind=NoDir) a b =
  let p = jointpathk [NoDir, a, outd; ind, b, NoDir] [style] in
  draw_arrow ?color ?pen p

let draw_label_arrow ?color ?pen ?(style=defaultjoint) ?(outd=NoDir) ?(ind=NoDir) ?pos lab a b =
  let p = jointpathk [NoDir, a, outd; ind, b, NoDir] [style] in
  draw_arrow ?color ?pen p ++
  label ?pos lab (Path.point 0.5 p)

let box_path ~style ~outd ~ind a b =
  let p = 
    jointpathk 
      [NoDir, Box.center a, outd; ind, Box.center b, NoDir] [style]
  in
  cut_after (bpath b) (cut_before (bpath a) p)

let box_arrow ?color ?pen ?(style=defaultjoint) ?(outd=NoDir) ?(ind=NoDir) a b =   
  draw_arrow ?color ?pen (box_path ?style ?outd ?ind a b)

let box_line ?color ?pen ?(style=defaultjoint) ?(outd=NoDir) ?(ind=NoDir) a b =   
  draw ?color ?pen (box_path ?style ?outd ?ind a b)

let box_label_line
    ?color ?pen ?(style=defaultjoint) ?(outd=NoDir) ?(ind=NoDir) ?pos lab a b =
  let p = jointpathk 
	[NoDir, Box.center a, outd; ind, Box.center b, NoDir] [style] in
  let p = cut_after (bpath b) (cut_before (bpath a) p) in
  draw ?color ?pen p ++
  label ?pos lab (Path.point 0.5 p)

let box_label_arrow 
    ?color ?pen ?(style=defaultjoint) ?(outd=NoDir) ?(ind=NoDir) ?pos lab a b =
  let p = jointpathk 
	[NoDir, Box.center a, outd; ind, Box.center b, NoDir] [style] in
  let p = cut_after (bpath b) (cut_before (bpath a) p) in
  draw_arrow ?color ?pen p ++
  label ?pos lab (Path.point 0.5 p)
