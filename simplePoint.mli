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

(* construct a point with the right measure *)
val bpp : float * float -> Point.t
val inp : float * float -> Point.t
val cmp : float * float -> Point.t
val mmp : float * float -> Point.t
val ptp : float * float -> Point.t

(* construct a list of points with the right measure *)
val map_bp : (float * float) list -> Point.t list
val map_in: (float * float) list -> Point.t list
val map_cm: (float * float) list -> Point.t list
val map_mm: (float * float) list -> Point.t list
val map_pt: (float * float) list -> Point.t list

(* might be useful to give another measure *)
val point : ?scale:(float -> Num.t) -> float * float -> Point.t
val ptlist : ?scale:(float -> Num.t) -> (float * float) list -> Point.t list

