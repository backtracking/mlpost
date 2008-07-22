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

let scaled a = TRScaled a
let rotated a = TRRotated a
let shifted a = TRShifted a
let slanted a = TRSlanted a
let xscaled a = TRXscaled a
let yscaled a = TRYscaled a
let zscaled a = TRZscaled a
let reflect p1 p2 = TRReflect (p1,p2)
let rotate_around p f = TRRotateAround (p,f)


let id = []
