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

type matrix = Types.matrix =  {
           xx : Num.t;
           yx : Num.t;
           xy : Num.t;
           yy : Num.t;
           x0 : Num.t;
           y0 : Num.t;
         }
let explicit t = mkTRMatrix t

(* applied the transformations in the order of the list *) 
let id = []
