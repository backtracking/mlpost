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
val p : Num.t * Num.t -> t
(* These functions create points of "unspecified" size, ie vectors
   to use with Vec for instance *)
val dir : float -> t
val up : t
val down : t
val left : t
val right : t

val unsafe : (Format.formatter -> unit) -> t

val north : Name.t -> t
val south : Name.t -> t
val west  : Name.t -> t
val east  : Name.t -> t
val north_west : Name.t -> t
val south_west : Name.t -> t
val north_east : Name.t -> t
val south_east : Name.t -> t

val segment : float -> t -> t -> t

(* operations on points *)
val add : t -> t -> t
val sub : t -> t -> t
val rotated : float -> t -> t

val print : Format.formatter -> t -> unit
