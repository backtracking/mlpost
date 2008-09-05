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

open Path

let simple ?style ?outd ?ind a b =
  let r,l = outd, ind in
   pathk ?style [knotp ?r a; knotp ?l b]

let no_head ?color ?pen ?dashed ?(angle = 60.) ?(size = Num.bp 4.) p dir =
  Command.nop

let simple_head ?color ?pen ?dashed ?(angle = 60.) ?(size = Num.bp 4.) p dir =
  let dir = Point.scale (Num.divn size (Point.length dir)) dir in
  let neg = Point.scale (Num.bp (-1.)) in
  let dir_a = neg (Point.rotate (angle /. 2.) dir) in
  let dir_b = neg (Point.rotate (-. angle /. 2.) dir) in
  let a = Point.add p dir_a in
  let b = Point.add p dir_b in
  Command.seq [
    Command.draw ?color ?pen ?dashed (Path.pathp [p; a]);
    Command.draw ?color ?pen ?dashed (Path.pathp [p; b]);
  ]

let draw ?style ?outd ?ind ?(feet = fun x -> no_head x)
    ?(head = fun x -> simple_head x) a b =
  let path = pathk ?style [knotp ?r: outd a; knotp ?l: ind b] in
  let head_dir = Point.sub (Path.point 1. path) (Path.point 0.99 path) in
  let feet_dir = Point.sub (Path.point 0. path) (Path.point 0.01 path) in
  Command.seq [
    feet a feet_dir;
    Command.draw path;
    head b head_dir;
  ]
