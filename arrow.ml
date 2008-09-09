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

let normalize p =
  Point.scale (Num.divn (Num.bp 1.) (Point.length p)) p

let neg = Point.scale (Num.bp (-1.))

type head = Point.t -> Point.t -> Command.t

let no_head p dir =
  Command.nop

let simple_head ?color ?pen ?dashed ?(angle = 60.) ?(size = Num.bp 4.) p dir =
  let dir = Point.scale size dir in
  let dir_a = neg (Point.rotate (angle /. 2.) dir) in
  let dir_b = neg (Point.rotate (-. angle /. 2.) dir) in
  let a = Point.add p dir_a in
  let b = Point.add p dir_b in
  Command.seq [
    Command.draw ?color ?pen ?dashed (Path.pathp [p; a]);
    Command.draw ?color ?pen ?dashed (Path.pathp [p; b]);
  ]

let draw ?style ?outd ?ind ?(foot = fun x -> no_head x)
    ?(head = fun x -> simple_head x) a b =
  let path = simple ?style ?outd ?ind a b in
  let head_dir = normalize (Path.direction 1. path) in
  let foot_dir = normalize (Path.direction 0. path) in
  Command.seq [
    foot a foot_dir;
    Command.draw path;
    head b head_dir;
  ]

let thick_path ?style ?outd ?ind ?(width = Num.bp 10.)
    ?(head_length = Num.multf 2. width)
    ?(head_width = head_length)
    a b =
  let path = simple ?style ?outd ?ind a b in
  let a_dir = normalize (Path.direction 0. path) in
  let a_normal = Point.rotate 90. a_dir in
  let a1 = Point.add (Point.scale (Num.divf width 2.) a_normal) a in
  let a2 = Point.add (Point.scale (Num.divf width (-2.)) a_normal) a in
  let b_dir = normalize (Path.direction 1. path) in
  let b_normal = Point.rotate 90. b_dir in
  let c = Point.add (Point.scale (Num.neg head_length) b_dir) b in
  let c1 = Point.add (Point.scale (Num.divf width 2.) b_normal) c in
  let c2 = Point.add (Point.scale (Num.divf width (-2.)) b_normal) c in
  let c1' = Point.add (Point.scale (Num.divf head_width 2.) b_normal) c in
  let c2' = Point.add (Point.scale (Num.divf head_width (-2.)) b_normal) c in
(*  let path_ac = simple ?style ?outd ?ind a c in
  let m = Path.point 0.5 path_ac in
  let m_dir = normalize (Path.direction 0.5 path_ac) in
  let m_dir2 = Point.scale (Num.bp 0.) m_dir in
  let m_normal = Point.rotate 90. m_dir in
  let m1 = Point.add (Point.scale (Num.divf width 2.) m_normal) m in
  let m2 = Point.add (Point.scale (Num.divf width (-2.)) m_normal) m in*)
  let path1 =
    pathk ~style: JCurve [
      knotp ~r: (Path.Vec a_dir) a1;
(*      knotp m1;*)
      knotp ~l: (Path.Vec b_dir) c1;
    ]
  in
  let path2 =
    pathk ~style: JCurve [
      knotp ~r: (Path.Vec (neg b_dir)) c2;
(*      knotp m2;*)
      knotp ~l: (Path.Vec (neg a_dir)) a2;
    ]
  in
  let path_head =
    pathk ~style: JLine [
      knotp c1';
      knotp b;
      knotp c2';
    ]
  in
  cycle ~style: JLine
    (append ~style: JLine (append ~style: JLine path1 path_head) path2)

let draw_thick ?style ?outd ?ind ?width ?head_length ?head_width a b =
  let p = thick_path ?style ?outd ?ind ?width ?head_length ?head_width a b in
  Command.draw p
