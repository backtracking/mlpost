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

(** The Mlpost Num module *)

type t = float
(** The Mlpost numeric type is just a float *)

(** {2 Conversion functions} *)
(** The base unit in Mlpost are bp. The following functions permit to specify
    values in other common units *)

val bp : float -> t
val pt : float -> t
val cm : float -> t
val mm : float -> t
val inch : float -> t

type scale = float -> t

module Scale : sig
  val bp : float -> scale
  val pt : float -> scale
  val cm : float -> scale
  val mm : float -> scale
  val inch : float -> scale
end
