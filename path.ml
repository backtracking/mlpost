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

module S = Point
include PrimPath

let knotp ?(l=NoDir) ?(r=NoDir) p = (l, p, r)

let knot ?(l) ?(r) ?(scale) p = knotp ?l (S.p ?scale p) ?r
let knotn ?(l) ?(r) p = knotp ?l (S.pt p) ?r

let cycle_tmp ?(dir=NoDir) ?(style=defaultjoint) p = PrimPath.cycle dir style p
let cycle = cycle_tmp
let concat ?(style=JCurve) p k = concat p style k

(* construct a path with a given style from a knot list *)
let pathk ?(style) ?(cycle) = function
  | [] -> failwith "empty path is not allowed"
  | (x::xs) ->
      let p = List.fold_left 
                 (fun p knot -> concat ?style p knot) (start x) xs
      in
        match cycle with
          | None -> p
          | Some style -> cycle_tmp ~style p

let pathp ?(style) ?(cycle) l =
  pathk ?style ?cycle
    (List.map (knotp) l)

let pathn ?(style) ?(cycle) l = pathp ?style ?cycle (List.map (Point.pt) l)

let path ?(style) ?(cycle) ?(scale) l =
  let sc = S.ptlist ?scale in pathp ?style ?cycle (sc l)

(* construct a path with knot list and joint list *)
let jointpathk lp lj =
  List.fold_left2 PrimPath.concat (start (List.hd lp)) lj (List.tl lp)

let jointpathp lp lj  = jointpathk (List.map (knotp) lp) lj
let jointpathn lp lj  = jointpathk (List.map knotn lp) lj
let jointpath ?(scale) lp lj  = jointpathk (List.map (knot ?scale) lp) lj

let append ?(style=JCurve) p1 p2 = append p1 style p2

let scale f p = transform [Transform.scaled f] p
let rotate f p = transform [Transform.rotated f] p
let shift pt path = transform [Transform.shifted pt] path
let yscale n p = transform [Transform.yscaled n] p
let xscale n p = transform [Transform.xscaled n] p
