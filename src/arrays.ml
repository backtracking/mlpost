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

open Command
open Num
open Point
open Path
open Box

type t = Box.t

let u = 12.
let height = u

let add o f b = match o with
  | None -> b
  | Some o -> add_post_draw (f o) b

let add_left pic b =
  Command.label ~pos:`Left pic (west b)

let create ?(u=u) ?pen ?left len =
  let width = float len *. u in
  let b = rect ?pen (empty ~width ~height ()) in
  add left add_left b

let xat b i =
  let w = xpart (east b) -. xpart (west b) in
  let len = floor (w /. u) in
  let f = w /. len in
  i *. f

let topat b i =
  Point.shift (north_west b) (Point.pt (xat b i, zero))
let botat b i =
  Point.shift (south_west b) (Point.pt (xat b i, zero))
let ctrat b i =
  Point.shift (west       b) (Point.pt (xat b i, zero))

let index ?(pos=`Top) i pic = add_post_draw @@ fun b ->
  let i = i +. 0.5 in
  let p = topat b i in
  Command.label ~pos pic p

let below ?(pos=`Bot) i pic = add_post_draw @@ fun b ->
  let i = i +. 0.5 in
  let p = botat b i in
  Command.label ~pos pic p

let cell ?(pos=`Center) i pic = add_post_draw @@ fun b ->
  let i = i +. 0.5 in
  let p = ctrat b i in
  Command.label ~pos pic p

let sep ?pen i = add_post_draw @@ fun b ->
  Path.draw ?pen (pathp [botat b i; topat b i])

let tick ?pen i = add_post_draw @@ fun b ->
  let p1 = botat b i in
  let p2 = Point.shift p1 (Point.pt (zero, 2. *. u)) in
  Path.draw ?pen (pathp [p1; p2])

let fill ?(sep=false) ?pen ?(color=Color.lightred) i j =
  add_pre_draw @@ fun b ->
  let p1 = botat b i in
  let p2 = topat b i in
  let p3 = topat b j in
  let p4 = botat b j in
  let p = pathp ~style:jLine ~cycle:jLine [p1;p2;p3;p4] in
  Path.fill ~color p ++
  if sep then Path.draw ?pen (pathp [botat b i; topat b i])
           ++ Path.draw ?pen (pathp [botat b j; topat b j]) else nop
