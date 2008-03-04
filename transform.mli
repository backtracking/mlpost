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

type t'

val scaled : ?scale:(float -> Num.t) -> float -> t'
val rotated : float -> t'
val shifted : Point.t -> t'
val slanted : Num.t -> t'
val xscaled : Num.t -> t'
val yscaled : Num.t -> t'
val zscaled : Point.t -> t'
val reflect : Point.t -> Point.t -> t'
val rotate_around : Point.t -> float -> t'

type t = t' list
val id : t

val print : Format.formatter -> t -> unit
