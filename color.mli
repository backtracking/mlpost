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

type spec =
  | RGB of float * float * float
  | Gray of float
      (* O-ary colors ^^ *)
  | Black
  | White
      (* primary colors *)
  | Red
  | Green
  | Blue
      (* secondary colors *)
  | Yellow
  | Cyan
  | Magenta
      (* er.. help me out there ! *)
  | Orange
  | Purple

val default : t
val make : spec -> t

val red : t
val orange : t
val blue : t
val purple : t

val print : Format.formatter -> t -> unit
