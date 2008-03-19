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

(** Colors *)

type t = Types.color 
  (** the abstract type of Colors *)

val default : t
  (** the default Color is black *)

val rgb : float -> float -> float -> t
  (** [rgb r g b] constructs the color that corresponds to the color code 
      RGB(r,g,b)  *)

(** {2 Predefined Colors} *)

val red : t
val orange : t
val blue : t
val purple : t
val gray : float -> t
val white : t
val black : t
val green : t
val cyan : t
val blue : t
val yellow : t
val magenta : t
val orange : t
val purple : t
