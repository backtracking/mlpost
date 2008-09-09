(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Johannes Kanig, Stephane Lescuyer                       *)
(*  and Jean-Christophe Filliatre                                         *)
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

type direction = Types.direction =
  | Vec of Point.t
  | Curl of float
  | NoDir 

type joint = Types.joint =
    JLine
  | JCurve
  | JCurveNoInflex
  | JTension of float * float
  | JControls of Point.t * Point.t

type knot = direction * Point.t * direction

(* the intention is to add new knots in front,
 * i. e. the list has to be reversed for printing *)
type t = path

let start k = PAKnot k
let concat p j k = PAConcat (k,j,p)
let cycle d j p = PACycle (d,j,p)
let append p1 j p2 = PAAppend (p1,j,p2)
let fullcircle = PAFullCircle
let halfcircle = PAHalfCircle
let quartercircle = PAQuarterCircle
let unitsquare = PAUnitSquare
let transform tr p = PATransformed (p,tr)
let cut_after p1 p2 = PACutAfter (p1, p2)
let cut_before p1 p2 = PACutBefore (p1, p2)
let build_cycle l = PABuildCycle l
let subpath f1 f2 p = PASub (f1, f2, p)

let point f p = PTPointOf (f, p)
let direction f p = PTDirectionOf (f, p)

let length p = NLength p

let defaultjoint = JCurve
