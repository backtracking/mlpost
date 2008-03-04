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

type t

type circle_style =
  | Padding of Num.t * Num.t (* dx , dy *)
  | Ratio of float (* dx / dy *)

val circle : ?style:circle_style -> Point.t -> Picture.t -> t
val rect : Point.t -> Picture.t -> t

val center : t -> Point.t
val north : t -> Point.t
val south : t -> Point.t
val west  : t -> Point.t
val east  : t -> Point.t 
val north_west : t -> Point.t
val south_west : t -> Point.t
val north_east : t -> Point.t
val south_east : t -> Point.t

val declare : Format.formatter -> t -> unit

val name : t -> Name.t

