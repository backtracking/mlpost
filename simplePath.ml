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

module S = SimplePoint
module P = Path

let knotp ?(l=P.NoDir) ?(r=P.NoDir) p = (l, p, r)

let knot ?(l) ?(r) ?(scale) p = knotp ?l (S.point ?scale p) ?r

let cycle_tmp ?(dir=P.NoDir) ?(style=P.JCurve) p = Path.cycle dir style p
let cycle = cycle_tmp
let concat ?(style=P.JCurve) p k = P.concat p style k

(* construct a path with a given style from a knot list *)
let pathk ?(style) ?(cycle) = function
  | [] -> failwith "empty path is not allowed"
  | (x::xs) ->
      let p = List.fold_left 
                 (fun p knot -> concat ?style p knot) (P.start x) xs
      in
        match cycle with
          | None -> p
          | Some style -> cycle_tmp ~style p

let pathp ?(style) ?(cycle) l =
  pathk ?style ?cycle
    (List.map (knotp) l)

let path ?(style) ?(cycle) ?(scale) l =
  let sc = S.ptlist ?scale in
    pathp ?style ?cycle (sc l)

(* construct a path with knot list and joint list *)
let jointpathk lp lj =
  List.fold_left2 P.concat (P.start (List.hd lp)) lj (List.tl lp)

let jointpathp lp lj  = jointpathk (List.map (knotp) lp) lj
let jointpath ?(scale) lp lj  = jointpathk (List.map (knot ?scale) lp) lj

