(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Johannes Kanig, Stephane Lescuyer                       *)
(*  Jean-Christophe Filliatre, Romain Bardou and Francois Bobot           *)
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

open Types
open Num
open Infix

type corner = Types.corner

type t = Types.point


(* angle in degrees *)
let dir f =  
  let angle = Num.deg2rad f in
  mkPTPair (mkF (cos angle)) (mkF (sin angle))

let up = mkPTPair zero one
let down = mkPTPair zero minus_one
let left = mkPTPair minus_one zero
let right = mkPTPair one zero

let simple_transform t str = mkPTTransformed t str

let xpart p = 
  match p.Hashcons.node with
  | PTPair (a,b) -> a
  | _ -> mkNXPart p

let ypart p = 
  match p.Hashcons.node with
  | PTPair (a,b) -> b
  | _ -> mkNYPart p

(* insert more sophisticated simplifications *)
let rec add p1 p2 = 
  match p1.Hashcons.node,p2.Hashcons.node with
    | PTPair (a1,b1), PTPair (a2,b2) -> mkPTPair (addn a1 a2) (addn b1 b2)
    | PTAdd (p1',p2'), _ -> add p1' (add p2' p2)
    | PTSub (p1',p2'), _ -> add p1' (sub p2 p2')
    | _, _ -> mkPTAdd p1 p2

and mult f p =
(*     if Num.is_zero f then PTPair (F 0.,F 0.) else *)
  match p.Hashcons.node with
    | PTPair (a,b) -> mkPTPair (multn f a) (multn f b)
    | PTMult (f', p) -> mult (multn f f') p
    | PTAdd (p1,p2) -> add (mult f p1) (mult f p2)
    | PTSub (p1,p2) -> sub (mult f p1) (mult f p2)
    | PTRotated (f', p) -> mkPTRotated f' (mult f p)
    | _ -> mkPTMult f p

and sub p1 p2 = 
  match p1.Hashcons.node,p2.Hashcons.node with
    | PTPair (a1,b1), PTPair (a2,b2) -> mkPTPair (subn a1 a2) (subn b1 b2)
    | PTAdd (p1',p2'), _ -> add p1' (sub p2' p2)
    | PTSub (p1',p2'), _ -> sub p1' (add p2' p2)
    | _, _ -> mkPTSub p1 p2

let shift = add
let scale = mult

let segment f p1 p2 = add (mult (mkF (1.-.f)) p1) (mult (mkF f) p2)
let rec rotate f p = 
  match p.Hashcons.node with
  | PTPair (a,b) -> 
      let angle = Num.deg2rad f in
      mkPTPair  
	((mkF (cos angle) */ a) -/ (mkF (sin angle) */ b))
        ((mkF (sin angle) */ a) +/ (mkF (cos angle) */ b))
  | PTAdd (p1, p2) -> add (rotate f p1) (rotate f p2)
  | PTSub (p1, p2) -> sub (rotate f p1) (rotate f p2)
  | PTRotated (f', p) -> mkPTRotated (f+.f') p
  | PTMult (f', p) -> mkPTMult f' (rotate f p)
  | _ -> mkPTRotated f p

(* rotate p2 around p1 *)
let rotate_around p1 f p2 = add p1 (rotate f (sub p2 p1))

let xscale f p = 
  match p.Hashcons.node with
  | PTPair (a,b) -> mkPTPair (mkNMult f a) b
  | _ -> simple_transform p (mkTRXscaled f)

let yscale f p = 
  match p.Hashcons.node with
  | PTPair (a,b) -> mkPTPair a (mkNMult f b)
  | _ -> simple_transform p (mkTRYscaled f)

let transform tr p =
  List.fold_left 
    (fun acc str -> 
       match str.Hashcons.node with
       | TRScaled f -> mult f acc
       | TRShifted p -> add acc p
       | TRRotated f -> rotate f acc
       | TRXscaled f -> xscale f acc
       | TRYscaled f -> yscale f acc
       | TRRotateAround (p,f) -> rotate_around p f acc
       | _ -> simple_transform acc str
    ) p tr
    
(* From simplePoint *)
let pmap f (a,b) = (f a, f b)

let pt (a,b) = mkPTPair a b

let p ?(scale=Num.bp) pr =
  pt (pmap scale pr)

let length p = gmean (xpart p) (ypart p)

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

let draw ?color ?pen t = 
  (* We don't use a default to avoid the output of 
     ... withcolor (0.00red+0.00green+0.00blue) withpen .... 
     for each command in the output file *)
    mkCommand (mkCDraw (mkPAofMPA (mkMPAKnot (mkKnot mkNoDir t mkNoDir))) color pen None)
