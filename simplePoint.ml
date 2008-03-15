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

include Point
let pmap f (a,b) = (f a, f b)

let p ?(scale=Num.bp) pr =
  Point.p (pmap scale pr)

let ptlist ?scale l = List.map (p ?scale) l

(* construct a point with the right measure *)
let bpp, inp, cmp, mmp, ptp = 
    p ~scale:Num.bp, 
    p ~scale:Num.inch, 
    p ~scale:Num.cm, 
    p ~scale:Num.mm, 
    p ~scale:Num.pt

(* construct a list of points with the right measure *)
let map_bp, map_in, map_cm, map_mm, map_pt =
  ptlist ~scale:Num.bp, 
  ptlist ~scale:Num.inch, 
  ptlist ~scale:Num.cm, 
  ptlist ~scale:Num.mm, 
  ptlist ~scale:Num.pt
