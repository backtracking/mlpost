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

(** Boxes *)

type t = Types.box
  (** the abstract type of boxes *)

(** {2 Creating boxes} *)

type circle_style = Types.box_circle_style =
  | Padding of Num.t * Num.t (** dx , dy *)
  | Ratio of float           (** dx / dy *)

val circle : ?style:circle_style -> Point.t -> Picture.t -> t
  (** [circle p pic] creates a circle box of center [p] and of contents
      [pic] *)

val rect : Point.t -> Picture.t -> t
  (** [rect p pic] creates a rectangular box of center [p] and of contents
      [pic] *)

(** {2 Special points of a box} *)

val center : t -> Point.t
val north : t -> Point.t
val south : t -> Point.t
val west  : t -> Point.t
val east  : t -> Point.t 
val north_west : t -> Point.t
val south_west : t -> Point.t
val north_east : t -> Point.t
val south_east : t -> Point.t


