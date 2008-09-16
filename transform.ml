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

type t' = transform

type t = t' list

let scaled a = mkTRScaled a
let rotated a = mkTRRotated a
let shifted a = mkTRShifted a
let slanted a = mkTRSlanted a
let xscaled a = mkTRXscaled a
let yscaled a = mkTRYscaled a
let zscaled a = mkTRZscaled a
let reflect p1 p2 = mkTRReflect p1 p2
let rotate_around p f = mkTRRotateAround p f

let id = []
