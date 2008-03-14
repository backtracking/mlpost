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

let circle ?style c p = BCircle (Name.create (), c, p, style)
let rect c p = BRect (Name.create (), c, p)

let center = function
  | BCircle (_, c, _, _) 
  | BRect (_, c, _) -> c

let north b = Point.north (name b)
let south b = Point.south (name b)
let west b = Point.west (name b)
let east b = Point.east (name b)
let north_west = function
  | BRect (n, _, _) -> Point.north_west n
  | BCircle _ -> invalid_arg "north_west: circle"
let north_east = function 
  | BRect (n, _, _) -> Point.north_east n
  | BCircle _ -> invalid_arg "north_east: circle"
let south_west = function 
  | BRect (n, _, _) -> Point.south_west n
  | BCircle _ -> invalid_arg "south_west: circle"
let south_east = function 
  | BRect (n, _, _) -> Point.south_east n
  | BCircle _ -> invalid_arg "south_east: circle"


