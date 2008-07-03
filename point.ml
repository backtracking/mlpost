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

type corner = Types.corner

type t = Types.point

let pt (a,b) = PTPair (a,b)

(* angle in degrees *)
let dir f =  
  let angle = Num.deg2rad f in
    PTPair (F (cos angle), F (sin angle))

let up = PTPair (F 0., F 1.)
let down = PTPair (F 0., F (-1.))
let left = PTPair (F (-1.), F 0.)
let right = PTPair (F 1., F 0.)

let simple_transform str = function
  | PTTransformed (p,tr) -> PTTransformed (p,str::tr)
  | _ as p -> PTTransformed (p,[str])

(* insert more sophisticated simplifications *)
let rec add p1 p2 = 
  match p1,p2 with
    | PTPair (a1,b1), PTPair (a2,b2) -> PTPair (NAdd(a1,a2), NAdd(b1,b2))
    | PTAdd (p1',p2'), _ -> add p1' (add p2' p2)
    | PTSub (p1',p2'), _ -> add p1' (sub p2 p2')
    | _, _ -> PTAdd (p1,p2)

and mult f p =
(*     if Num.is_zero f then PTPair (F 0.,F 0.) else *)
  match p with
    | PTPair (a,b) -> PTPair (NMult(f, a), NMult(f, b))
    | PTMult (f', p) -> mult (NMult(f,f')) p
    | PTAdd (p1,p2) -> add (mult f p1) (mult f p2)
    | PTSub (p1,p2) -> sub (mult f p1) (mult f p2)
    | PTRotated (f', p) -> PTRotated (f', mult f p)
    | _ as p -> PTMult (f,p)

and sub p1 p2 = 
  match p1,p2 with
    | PTPair (a1,b1), PTPair (a2,b2) -> PTPair (NMinus(a1, a2), NMinus(b1, b2))
    | PTAdd (p1',p2'), _ -> add p1' (sub p2' p2)
    | PTSub (p1',p2'), _ -> sub p1' (add p2' p2)
    | _, _ -> PTSub (p1,p2)


let segment f p1 p2 = add (mult (F (1.-.f)) p1) (mult (F f) p2)
let rec rotated f = function
  | PTPair (a,b) -> 
      let angle = Num.deg2rad f in
        PTPair (NMinus(NMult(F (cos(angle)), a), NMult(F (sin(angle)), b)),
		NAdd(NMult(F (sin(angle)), a), NMult(F (cos(angle)), b)))
  | PTAdd (p1, p2) -> add (rotated f p1) (rotated f p2)
  | PTSub (p1, p2) -> sub (rotated f p1) (rotated f p2)
  | PTRotated (f', p) -> PTRotated (f+.f', p)
  | PTMult (f', p) -> PTMult(f', rotated f p)
  | _ as p -> PTRotated (f, p)

(* rotate p2 around p1 *)
let rotate_around p1 f p2 = add p1 (rotated f (sub p2 p1))

let xscaled f = function
  | PTPair (a,b) -> PTPair (NMult(f,a), b)
  | _ as p -> simple_transform (TRXscaled f) p

let yscaled f = function
  | PTPair (a,b) -> PTPair (a, NMult(f,b))
  | _ as p -> simple_transform (TRYscaled f) p

let transform tr p =
  List.fold_left 
    (fun acc -> function
       | TRScaled f -> mult f acc
       | TRShifted p -> add acc p
       | TRRotated f -> rotated f acc
       | TRXscaled f -> xscaled f acc
       | TRYscaled f -> yscaled f acc
       | TRRotateAround (p,f) -> rotate_around p f acc
       | _ as str -> simple_transform str acc
    ) p tr
    
(* From simplePoint *)
let pmap f (a,b) = (f a, f b)

let p ?(scale=Num.bp) pr =
  pt (pmap scale pr)

let lengthpoint p = PTLength p

let p ?(scale=Num.bp) pr =
  pt (pmap scale pr)

let origin = p (0.,0.)


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
