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
  | BoxCorner of Name.t * corner
  | Unsafe of (Format.formatter -> unit)
  | Add of t * t
  | Sub of t * t
  | Mult of float * t
  | Rotated of float * t

let p (a,b) = Ppair (a,b)

(* angle in degrees *)
let dir f =  
  let angle = Misc.deg2rad f in
    Ppair (cos angle, sin angle)

let up = Ppair (0.,1.)
let down = Ppair (0.,-1.)
let left = Ppair (-1.,0.)
let right = Ppair (1.,0.)
let north n = BoxCorner (n, N)
let south n = BoxCorner (n, S)
let west n = BoxCorner (n, W)
let east n = BoxCorner (n, E)
let north_west n = BoxCorner (n, NW)
let north_east n = BoxCorner (n, NE)
let south_west n = BoxCorner (n, SW)
let south_east n = BoxCorner (n, SE)
let unsafe f = Unsafe f


(* insert more sophisticated simplifications *)
let rec add p1 p2 = 
  match p1,p2 with
    | Ppair (a1,b1), Ppair (a2,b2) -> Ppair (a1 +. a2, b1 +. b2)
    | Add (p1',p2'), _ -> add p1' (add p2' p2)
    | Sub (p1',p2'), _ -> add p1' (sub p2 p2')
    | _, Add (p1',p2') -> add p1' (add p1 p2')
    | _, Sub (p1',p2') -> add p1' (sub p1 p2')
    | _, _ -> Add (p1,p2)

and mult f = function
  | Ppair (a,b) -> Ppair (f *. a, f *. b)
  | Mult (f', p) -> mult (f *. f') p
  | Add (p1,p2) -> add (mult f p1) (mult f p2)
  | Sub (p1,p2) -> sub (mult f p1) (mult f p2)
  | Rotated (f', p) -> Rotated (f', mult f p)
  | _ as p -> Mult (f,p)

and sub p1 p2 = 
  match p1,p2 with
    | Ppair (a1,b1), Ppair (a2,b2) -> Ppair (a1 -. a2, b1 -. b2)
    | Add (p1',p2'), _ -> add p1' (sub p2' p2)
    | Sub (p1',p2'), _ -> sub p1' (add p2' p2)
    | _, Add (p1',p2') -> sub (sub p1 p1') p2'
    | _, Sub (p1',p2') -> add (sub p1 p1') p2'
    | _, _ -> Sub (p1,p2)

let segment f p1 p2 = add (mult (1.-.f) p1) (mult f p2)
let rec rotated f = function
  | Ppair (a,b) -> 
      let angle = Misc.deg2rad f in
        Ppair (cos(angle) *. a -. sin(angle) *. b,
               sin(angle) *. a -. cos(angle) *. b)
  | Add (p1, p2) -> add (rotated f p1) (rotated f p2)
  | Sub (p1, p2) -> sub (rotated f p1) (rotated f p2)
  | Rotated (f', p) -> Rotated (f+.f', p)
  | Mult (f', p) -> Mult(f', rotated f p)
  | _ as p -> Rotated (f, p)

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
  | Ppair (m,n) -> F.fprintf fmt "(%a,%a)" Num.print m Num.print n
  | BoxCorner (n, d) -> F.fprintf fmt "%a.%a" Name.print n print_corner d
  | Unsafe f -> f fmt
  | Add (p1, p2) -> F.fprintf fmt "(%a + %a)" print p1 print p2
  | Sub (p1, p2) -> F.fprintf fmt "(%a - %a)" print p1 print p2
  | Mult (f, p) -> F.fprintf fmt "(%a * %a)" Num.print_float f print p
  | Rotated (f, p) ->  
      F.fprintf fmt "(%a rotated %a)" print p Num.print_float f
