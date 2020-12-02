(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Johannes Kanig, Stephane Lescuyer                       *)
(*  Jean-Christophe Filliatre, Romain Bardou and Francois Bobot           *)
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

open Path
open Box
open Command

(*  puts labels at given points with given text *)
let dotlabels ?(pos = `Center) ls lp =
  seq (List.map2 (fun s p -> dotlabel ~pos (Picture.tex s) p) ls lp)

let draw_simple_arrow ?color ?pen ?dashed ?style ?outd ?ind ?sep a b =
  Arrow.simple ?color ?pen ?dashed
    (Box.strip ?sep (Arrow.simple_point_point ?style ?outd ?ind a b))

let draw_label_arrow ?color ?pen ?dashed ?style ?outd ?ind ?pos ?sep lab a b =
  let p = Arrow.simple_point_point ?style ?outd ?ind ?sep a b in
  Arrow.simple ?color ?pen ?dashed p ++ label ?pos lab (Path.point 0.5 p)

let draw_labelbox_arrow ?color ?pen ?dashed ?style ?outd ?ind ?pos ?sep lab a b
    =
  draw_label_arrow ?color ?pen ?dashed ?style ?outd ?ind ?pos ?sep
    (Picture.make (Box.draw lab))
    a b

let subboxes within a b =
  match within with None -> (a, b) | Some x -> (Box.sub a x, Box.sub b x)

let box_arrow ?within ?color ?pen ?dashed ?style ?outd ?ind ?sep a b =
  let a, b = subboxes within a b in
  Arrow.simple ?color ?pen ?dashed (Box.cpath ?style ?outd ?ind ?sep a b)

let box_line ?within ?color ?pen ?dashed ?style ?outd ?ind ?sep a b =
  let a, b = subboxes within a b in
  draw ?color ?pen ?dashed (Box.cpath ?style ?outd ?ind ?sep a b)

let box_point_line ?within ?color ?pen ?dashed ?style ?outd ?ind ?sep a b =
  let a = match within with None -> a | Some x -> Box.sub a x in
  draw ?color ?pen ?dashed (Box.cpath_left ?style ?outd ?ind ?sep a b)

let point_box_line ?within ?color ?pen ?dashed ?style ?outd ?ind ?sep a b =
  let b = match within with None -> b | Some x -> Box.sub b x in
  draw ?color ?pen ?dashed (Box.cpath_right ?style ?outd ?ind ?sep a b)

let box_label_line ?within ?color ?pen ?dashed ?style ?outd ?ind ?sep ?pos lab a
    b =
  let a, b = subboxes within a b in
  let p = Box.cpath ?style ?outd ?ind ?sep a b in
  draw ?color ?pen ?dashed p ++ label ?pos lab (Path.point 0.5 p)

let box_label_arrow ?within ?color ?pen ?dashed ?style ?outd ?ind ?sep ?pos lab
    a b =
  let a, b = subboxes within a b in
  let p = Box.cpath ?style ?outd ?ind ?sep a b in
  Arrow.simple ?color ?pen ?dashed p ++ label ?pos lab (Path.point 0.5 p)

(* TODO unify all these functions *)
let box_labelbox_arrow ?within ?color ?pen ?dashed ?style ?outd ?ind ?sep ?pos
    lab a b =
  box_label_arrow ?within ?color ?pen ?dashed ?style ?outd ?ind ?sep ?pos
    (Picture.make (Box.draw lab))
    a b

let pointer_start_pen = Pen.scale (Num.bp 4.) Pen.circle

let box_pointer_arrow ?within ?color ?pen ?dashed ?style ?outd ?ind a b =
  let a, b = subboxes within a b in
  let r, l = (outd, ind) in
  let p = Path.pathk ?style [ Path.knotp ?r (ctr a); Path.knotp ?l (ctr b) ] in
  let p = Path.cut_after (bpath b) p in
  Command.draw ~pen:pointer_start_pen (pathp [ Path.point 0. p ])
  ++ Arrow.simple ?color ?pen ?dashed p

(***

let hboxjoin ?color ?pen ?dashed ?dx ?dy ?pos ?spacing pl =
  (* align the pictures in pl, put them in boxes and connect these boxes *)
  let bl = Box.halign_to_box ?dx ?pos ?spacing pl in
    match bl with
    | [] -> nop
    | hd::tl ->
        let cmd,_ =
          List.fold_left
          (fun (cmd,b1) b2 ->
            Box.draw b2 ++ box_arrow ?color ?pen ?dashed b1 b2 ++ cmd,b2 )
          (Box.draw hd,hd) tl
        in
          cmd

***)
