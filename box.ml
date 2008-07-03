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

open Types

type circle_style = box_circle_style =
  | Padding of Num.t * Num.t (* dx , dy *)
  | Ratio of float (* dx / dy *)

type t = box

let circle ?style c p = BCircle (c, p, style)
let rect c p = BRect (c, p)

let center = function
  | BCircle (c, _, _) 
  | BRect (c, _) -> c

let north b = PTBoxCorner (b,N)
let south b = PTBoxCorner (b,S)
let west b = PTBoxCorner (b,W)
let east b = PTBoxCorner (b,E)
let north_west = function
  | BRect _ as b -> PTBoxCorner (b,NW)
  | BCircle (c,_,_) as b -> Point.rotate_around c 45. (PTBoxCorner (b,N))
let north_east = function 
  | BRect _ as b -> PTBoxCorner (b,NE)
  | BCircle (c,_,_) as b -> Point.rotate_around c 45. (PTBoxCorner (b,E))
let south_west = function 
  | BRect _ as b -> PTBoxCorner (b,SW)
  | BCircle (c,_,_) as b -> Point.rotate_around c 45.(PTBoxCorner (b,W))
let south_east = function 
  | BRect _ as b -> PTBoxCorner (b,SE)
  | BCircle (c,_,_) as b -> Point.rotate_around c 45.(PTBoxCorner (b,S))

let bpath b = PABoxBPath b
