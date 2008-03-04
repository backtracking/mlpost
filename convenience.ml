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

module P = Path
open Helpers

let path ?(style=P.JCurve) ?(cycle) ?(scale=Num.bp) l  =
  let sc = List.map (fun pr -> Point.p (pmap scale pr)) in
  let p = point_fold style (sc l) in
    match cycle with
      | None -> p
      | Some cst -> P.cycle P.NoDir cst p

let p ?(l=P.NoDir) ?(r=P.NoDir) ?(scale=Num.bp) (a,b) =
  let s pair = Point.p (pmap scale pair) in
    (l, s (a, b), r)

let draw ?(style) ?(cycle) ?(scale) ?(color) ?(pen) l =
   Command.draw ?color ?pen (path ?style ?cycle ?scale l)

let jointpath ?(scale=Num.bp) lp lj  =
  let s pair =  Point.p (pmap scale pair) in
    jointpath (List.map (fun (a,b) -> (P.NoDir, s (a, b), P.NoDir)) lp) lj

