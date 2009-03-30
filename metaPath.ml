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

module S = Point
include PrimPath

open Types
type t = metapath
type path = Types.path
let knotp ?(l=defaultdir) ?(r=defaultdir) p = Types.mkKnot l p r 

let knot ?(l) ?(r) ?(scale) p = knotp ?l (S.p ?scale p) ?r
let knotn ?(l) ?(r) p = knotp ?l (S.pt p) ?r

let knotlist = List.map (fun (x,y,z) -> Types.mkKnot x y z)

let cycle ?(dir=defaultdir) ?(style=defaultjoint) p = PrimPath.metacycle dir style p

let concat ?(style=defaultjoint) p k = concat p style k

(* construct a path with a given style from a knot list *)
let pathk ?(style) = function
  | [] -> failwith "empty path is not allowed"
  | (x::xs) ->
      List.fold_left 
                 (fun p knot -> concat ?style p knot) (start x) xs

let pathp ?(style) l =
  pathk ?style 
    (List.map (knotp) l)

let pathn ?(style) l = pathp ?style (List.map (Point.pt) l)

let path ?(style) ?(scale) l =
  let sc = S.ptlist ?scale in pathp ?style (sc l)

(* construct a path with knot list and joint list *)
let jointpathk lp lj =
  try
    List.fold_left2 PrimPath.concat (start (List.hd lp)) lj (List.tl lp)
  with Invalid_argument _ -> invalid_arg "jointpathk : the list of knot must \
be one more than the list of join"

let jointpathp lp lj  = jointpathk (List.map (knotp) lp) lj
let jointpathn lp lj  = jointpathk (List.map knotn lp) lj
let jointpath ?(scale) lp lj  = jointpathk (List.map (knot ?scale) lp) lj

let append ?(style=defaultjoint) p1 p2 = append p1 style p2
