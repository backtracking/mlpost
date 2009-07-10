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
open Transform

type t = Types.pen

let transform tr p = 
  List.fold_left mkPenTransformed p tr

let default = 
  mkPenTransformed mkPenCircle (scaled (mkF 0.5))
let circle = mkPenCircle
let square = mkPenSquare
let from_path p = mkPenFromPath p

let scale f p = transform [Transform.scaled f] p
let rotate f p = transform [Transform.rotated f] p
let shift pt path = transform [Transform.shifted pt] path
let yscale n p = transform [Transform.yscaled n] p
let xscale n p = transform [Transform.xscaled n] p
