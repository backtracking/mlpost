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

module F = Format

type corner = N | S | W | E | NE | NW | SW | SE

type t = 
  | Ppair of Num.t * Num.t
  | Dir of float
  | Up
  | Down
  | Left
  | Right
  | BoxCorner of Name.t * corner
  | Unsafe of (Format.formatter -> unit)
  | Segment of float * t * t
  | Add of t * t
  | Sub of t * t
  | Rotated of float * t

let p (a,b) = Ppair (a,b)
let dir f = Dir f
let up = Up
let down = Down
let left = Left
let right = Right
let north n = BoxCorner (n, N)
let south n = BoxCorner (n, S)
let west n = BoxCorner (n, W)
let east n = BoxCorner (n, E)
let north_west n = BoxCorner (n, NW)
let north_east n = BoxCorner (n, NE)
let south_west n = BoxCorner (n, SW)
let south_east n = BoxCorner (n, SE)
let unsafe f = Unsafe f
let segment f p1 p2 = Segment (f,p1,p2)
let add p1 p2 = Add (p1, p2)
let sub p1 p2 = Sub (p1, p2)
let rotated f p = Rotated (f, p)

let print_corner fmt = function
  | N -> F.fprintf fmt "n"
  | S -> F.fprintf fmt "s"
  | W -> F.fprintf fmt "w"
  | E -> F.fprintf fmt "e"
  | NW -> F.fprintf fmt "nw"
  | NE -> F.fprintf fmt "ne"
  | SW -> F.fprintf fmt "sw"
  | SE -> F.fprintf fmt "se"

let rec print fmt = function
  | Up -> F.fprintf fmt "up"
  | Down -> F.fprintf fmt "down"
  | Left -> F.fprintf fmt "left"
  | Right -> F.fprintf fmt "right"
  | Dir d -> F.fprintf fmt "dir %a" Num.print_float d
  | Ppair (m,n) -> F.fprintf fmt "(%a,%a)" Num.print m Num.print n
  | BoxCorner (n, d) -> F.fprintf fmt "%a.%a" Name.print n print_corner d
  | Unsafe f -> f fmt
  | Segment (f,p1,p2) -> 
      F.fprintf fmt "%a[%a,%a]" Num.print_float f print p1 print p2
  | Add (p1, p2) -> F.fprintf fmt "(%a + %a)" print p1 print p2
  | Sub (p1, p2) -> F.fprintf fmt "(%a - %a)" print p1 print p2
  | Rotated (f, p) ->  
      F.fprintf fmt "(%a rotated %a)" print p Num.print_float f
