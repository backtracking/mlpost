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

(* Extended arrows. *)

let normalize p =
  Point.scale (Num.divn (Num.bp 1.) (Point.length p)) p

let neg = Point.scale (Num.bp (-1.))

let direction_on_path f p =
  Path.directionn (Num.multf f (Path.length p)) p

let point_on_path f p =
  Path.pointn (Num.multf f (Path.length p)) p

let subpath_01 f t p =
  let l = Path.length p in
  let f = Num.multf f l in
  let t = Num.multf t l in
  Path.subpathn f t p

(* Atoms *)

type line = {
  dashed: Types.dash option;
  color: Types.color option;
  pen: Types.pen option;
  from_point: float;
  to_point: float;
  dist: Num.t;
}

type head = Point.t -> Point.t -> Command.t * Path.t

type belt = {
  clip: bool;
  rev: bool;
  point: float;
  head: head;
}

type kind = {
  lines: line list;
  belts: belt list;
}

let empty = {
  lines = [];
  belts = [];
}

let add_line ?dashed ?color ?pen ?(from_point = 0.) ?(to_point = 1.)
    ?(dist = Num.bp 0.) kind =
  { kind with lines = {
      dashed = dashed;
      color = color;
      pen = pen;
      from_point = from_point;
      to_point = to_point;
      dist = dist;
    } :: kind.lines }

let head_classic_points ?(angle = 60.) ?(size = Num.bp 4.) p dir =
  let dir = Point.scale size dir in
  let dir_a = neg (Point.rotate (angle /. 2.) dir) in
  let dir_b = neg (Point.rotate (-. angle /. 2.) dir) in
  let a = Point.add p dir_a in
  let b = Point.add p dir_b in
  a, b

let head_classic ?color ?pen ?dashed ?angle ?size p dir =
  let a, b = head_classic_points ?angle ?size p dir in
  let path = Path.pathp ~style: Path.jLine [a; p; b] in
  Command.draw ?color ?pen ?dashed path, path

let head_triangle ?color ?pen ?dashed ?angle ?size p dir =
  let a, b = head_classic_points ?angle ?size p dir in
  let path = Path.pathp ~style: Path.jLine ~cycle: Path.jLine [a; p; b] in
  let clipping_path = Path.pathp ~style: Path.jLine [a; b] in
  Command.draw ?color ?pen ?dashed path, clipping_path

let head_triangle_full ?color ?angle ?size p dir =
  let a, b = head_classic_points ?angle ?size p dir in
  let path = Path.pathp ~style: Path.jLine ~cycle: Path.jLine [a; p; b] in
  let clipping_path = Path.pathp ~style: Path.jLine [a; b] in
  Command.fill ?color path, clipping_path

let add_belt ?(clip = false) ?(rev = false) ?(point = 0.5)
    ?(head = fun x -> head_classic x) kind =
  { kind with belts = {
      clip = clip;
      rev = rev;
      point = point;
      head = head;
    } :: kind.belts }

let add_head ?head kind = add_belt ~clip: true ~point: 1. ?head kind

let add_foot ?head kind = add_belt ~clip: true ~rev: true ~point: 0. ?head kind

(* Compute the path of a line along an arrow path.
   Return the line (unchanged) and the computed path. *)
let make_arrow_line path line =
  (* TODO: use line.dist *)
  let path =
    if line.from_point <> 0. || line.to_point <> 1. then
      subpath_01 line.from_point line.to_point path
    else path
  in
  line, path

(* Compute the command and the clipping path of a belt along an arrow path.
   Return the belt (unchanged), the command and the clipping path. *)
let make_arrow_belt path belt =
  let p = point_on_path belt.point path in
  let d = normalize (direction_on_path belt.point path) in
  let d = if belt.rev then neg d else d in
  let command, clipping_path = belt.head p d in
  belt, command, clipping_path

(* Clip a line with a belt clipping path if needed. *)
let clip_line_with_belt (line, line_path) (belt, _, clipping_path) =
  let cut =
    if belt.clip then
      (if belt.rev then Path.cut_before else Path.cut_after) clipping_path
    else fun x -> x
  in
  line, cut line_path

(* Compute the command to draw a line. *)
let draw_line (line, line_path) =
  Command.draw ?color: line.color ?pen: line.pen ?dashed: line.dashed line_path

let classic = add_head (add_line empty)
let triangle = add_head ~head: head_triangle (add_line empty)
let triangle_full = add_head ~head: head_triangle_full (add_line empty)

let draw ?(kind = triangle_full) ?tex ?(pos = 0.5) ?anchor path =
  let lines, belts = kind.lines, kind.belts in
  let lines = List.map (make_arrow_line path) lines in
  let belts = List.map (make_arrow_belt path) belts in
  let lines =
    List.map (fun line -> List.fold_left clip_line_with_belt line belts) lines in
  let lines = List.map draw_line lines in
  let belts = List.map (fun (_, x, _) -> x) belts in
  let labels = match tex with
    | None -> []
    | Some tex ->
        [Command.label ?pos: anchor (Picture.tex tex) (point_on_path pos path)]
  in
  Command.seq (lines @ belts @ labels)

(* Instances *)

let point_to_point ?kind ?tex ?pos ?anchor ?outd ?ind a b =
  let r, l = outd, ind in
  draw ?kind ?tex ?pos ?anchor (Path.pathk [Path.knotp ?r a; Path.knotp ?l b])

let box_to_box ?kind ?tex ?pos ?anchor ?outd ?ind a b =
  draw ?kind ?tex ?pos ?anchor (Box.cpath ?outd ?ind a b)

let box_to_point ?kind ?tex ?pos ?anchor ?outd ?ind a b =
  draw ?kind ?tex ?pos ?anchor (Box.cpath_left ?outd ?ind a b)

let point_to_box ?kind ?tex ?pos ?anchor ?outd ?ind a b =
  draw ?kind ?tex ?pos ?anchor (Box.cpath_right ?outd ?ind a b)

(*******************************************************************************)
(*                                 To be sorted                                *)
(*******************************************************************************)

open Path

let simple_point_point ?style ?outd ?ind a b =
  let r,l = outd, ind in
  pathk ?style [knotp ?r a; knotp ?l b]


let normalize p =
  Point.scale (Num.divn (Num.bp 1.) (Point.length p)) p

let neg = Point.scale (Num.bp (-1.))

let thick_path ?style ?outd ?ind ?(width = Num.bp 10.)
    ?(head_length = Num.multf 2. width)
    ?(head_width = head_length)
    a b =
  let path = simple_point_point ?style ?outd ?ind a b in
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
    pathk ~style:jCurve [
      knotp ~r: (vec a_dir) a1;
(*      knotp m1;*)
      knotp ~l: (vec b_dir) c1;
    ]
  in
  let path2 =
    pathk ~style:jCurve [
      knotp ~r: (vec (neg b_dir)) c2;
(*      knotp m2;*)
      knotp ~l: (vec (neg a_dir)) a2;
    ]
  in
  let path_head =
    pathk ~style:jLine [
      knotp c1';
      knotp b;
      knotp c2';
    ]
  in
  cycle ~style:jLine
    (append ~style:jLine (append ~style:jLine path1 path_head) path2)

let draw_thick ?style ?(boxed=true) ?line_color ?fill_color ?outd ?ind ?width
    ?head_length ?head_width a b =
  let p = thick_path ?style ?outd ?ind ?width ?head_length ?head_width a b in
  let draw_cmd =
    if boxed then Command.draw ?color:line_color p else Command.nop
  in
  let fill_cmd =
    match fill_color with
      | None -> Command.nop
      | Some c -> Command.fill ~color:c p
  in
  Command.append fill_cmd draw_cmd

let simple ?color ?pen ?dashed p =
  let kind = 
    add_head 
      ~head:(head_triangle_full ?color) 
      (add_line ?dashed ?color ?pen empty) in
  draw ~kind p

