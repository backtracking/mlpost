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

type direction = Types.direction 

let vec = mkVec
let curl = mkCurl
let noDir = mkNoDir

type joint = Types.joint 

let jLine = mkJLine
let jCurve = mkJCurve
let jCurveNoInflex = mkJCurveNoInflex
let jTension = mkJTension
let jControls = mkJControls

type knot = Types.knot

(* the intention is to add new knots in front,
 * i. e. the list has to be reversed for printing *)
type t = path

let start k = mkPAKnot k
let concat p j k = mkPAConcat k j p
let cycle d j p = mkPACycle d j p
let append p1 j p2 = mkPAAppend p1 j p2
let fullcircle = mkPAFullCircle
let halfcircle = mkPAHalfCircle
let quartercircle = mkPAQuarterCircle
let unitsquare = mkPAUnitSquare
let transform tr p = List.fold_left mkPATransformed p tr
let cut_after p1 p2 = mkPACutAfter p1 p2
let cut_before p1 p2 = mkPACutBefore p1 p2
let build_cycle l = mkPABuildCycle l
let subpath f1 f2 p = mkPASub f1 f2 p

let point f p = mkPTPointOf f p
let direction f p = mkPTDirectionOf f p

let length p = mkNLength p

let defaultjoint = jCurve
let defaultdir = noDir
