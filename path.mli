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

type direction = PrimPath.direction =
  | Vec of Point.t
  | Curl of float
  | NoDir 

type joint = PrimPath.joint =
  | JLine
  | JCurve
  | JCurveNoInflex
  | JTension of float * float
  | JControls of Point.t * Point.t

type knot = PrimPath.knot

type t = PrimPath.t
(** build a knot from a pair of floats *)
val knot :
    ?l:PrimPath.direction -> ?r:PrimPath.direction -> 
      ?scale:(float -> Num.t) -> float * float -> PrimPath.knot
(** build a knot from a point *)
val knotp :
    ?l:PrimPath.direction -> ?r:PrimPath.direction -> Point.t -> PrimPath.knot

(** build a path from a list of floatpairs *)
val path : 
    ?style:PrimPath.joint -> ?cycle:PrimPath.joint -> ?scale:(float -> Num.t) -> 
      (float * float) list -> PrimPath.t

(** build a path from a knot list *)
val pathk :
    ?style:PrimPath.joint -> ?cycle:PrimPath.joint -> PrimPath.knot list -> PrimPath.t

(** build a path from a point list *)
val pathp :
    ?style:PrimPath.joint -> ?cycle:PrimPath.joint -> Point.t list -> PrimPath.t

(** build a path from [n] knots and [n-1] joints *)
val jointpathk : PrimPath.knot list -> PrimPath.joint list -> PrimPath.t
(** build a path from [n] points and [n-1] joints, with default directions *)
val jointpathp : Point.t list -> PrimPath.joint list -> PrimPath.t

(** build a path from [n] float_pairs and [n-1] joints, with default 
* directions *)
val jointpath : 
    ?scale:(float -> Num.t) -> (float * float) list -> 
      PrimPath.joint list -> PrimPath.t

(** the same functions as in [PrimPath] but with labels *)
val cycle : ?dir:PrimPath.direction -> ?style:PrimPath.joint -> PrimPath.t -> PrimPath.t
val concat : ?style:PrimPath.joint -> PrimPath.t -> PrimPath.knot -> PrimPath.t
val start : knot -> t
val append : ?style:PrimPath.joint -> t -> t -> t
val subpath : float -> float -> t -> t
val fullcircle : t
val halfcircle : t
val quartercircle: t
val unitsquare: t

val transform : Transform.t -> t -> t

val bpath : Box.t -> t

val point : float -> t -> Point.t

val cut_after : t -> t -> t
val cut_before: t -> t -> t
val build_cycle : t list -> t

