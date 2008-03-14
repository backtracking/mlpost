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

module F = Format

type corner = Types.corner

type t = Types.point

let p (a,b) = PTPair (a,b)

(* angle in degrees *)
let dir f =  
  let angle = Misc.deg2rad f in
    PTPair (cos angle, sin angle)

let up = PTPair (0.,1.)
let down = PTPair (0.,-1.)
let left = PTPair (-1.,0.)
let right = PTPair (1.,0.)
let north n = PTBoxCorner (n, N)
let south n = PTBoxCorner (n, S)
let west n = PTBoxCorner (n, W)
let east n = PTBoxCorner (n, E)
let north_west n = PTBoxCorner (n, NW)
let north_east n = PTBoxCorner (n, NE)
let south_west n = PTBoxCorner (n, SW)
let south_east n = PTBoxCorner (n, SE)


(* insert more sophisticated simplifications *)
let rec add p1 p2 = 
  match p1,p2 with
    | PTPair (a1,b1), PTPair (a2,b2) -> PTPair (a1 +. a2, b1 +. b2)
    | PTAdd (p1',p2'), _ -> add p1' (add p2' p2)
    | PTSub (p1',p2'), _ -> add p1' (sub p2 p2')
    | _, PTAdd (p1',p2') -> add p1' (add p1 p2')
    | _, PTSub (p1',p2') -> add p1' (sub p1 p2')
    | _, _ -> PTAdd (p1,p2)

and mult f = function
  | PTPair (a,b) -> PTPair (f *. a, f *. b)
  | PTMult (f', p) -> mult (f *. f') p
  | PTAdd (p1,p2) -> add (mult f p1) (mult f p2)
  | PTSub (p1,p2) -> sub (mult f p1) (mult f p2)
  | PTRotated (f', p) -> PTRotated (f', mult f p)
  | _ as p -> PTMult (f,p)

and sub p1 p2 = 
  match p1,p2 with
    | PTPair (a1,b1), PTPair (a2,b2) -> PTPair (a1 -. a2, b1 -. b2)
    | PTAdd (p1',p2'), _ -> add p1' (sub p2' p2)
    | PTSub (p1',p2'), _ -> sub p1' (add p2' p2)
    | _, PTAdd (p1',p2') -> sub (sub p1 p1') p2'
    | _, PTSub (p1',p2') -> add (sub p1 p1') p2'
    | _, _ -> PTSub (p1,p2)

let segment f p1 p2 = add (mult (1.-.f) p1) (mult f p2)
let rec rotated f = function
  | PTPair (a,b) -> 
      let angle = Misc.deg2rad f in
        PTPair (cos(angle) *. a -. sin(angle) *. b,
               sin(angle) *. a -. cos(angle) *. b)
  | PTAdd (p1, p2) -> add (rotated f p1) (rotated f p2)
  | PTSub (p1, p2) -> sub (rotated f p1) (rotated f p2)
  | PTRotated (f', p) -> PTRotated (f+.f', p)
  | PTMult (f', p) -> PTMult(f', rotated f p)
  | _ as p -> PTRotated (f, p)

