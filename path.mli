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

(** Paths *)

(** A direction, which is useful to put constraints on paths on points
  {ul {- [Vec p] defines a direction by a point (interpreted as a vector)}
      {- [Curl f] Change the curling factor of the extremity of a path}
      {- [NoDir] means no particular direction} } *)
type direction = PrimPath.direction =
  | Vec of Point.t
  | Curl of float
  | NoDir 

(** A [knot] is simply a point with an incoming and outgoing direction constraint
*)
type knot = direction * Point.t * direction

(** A joint is the connection between two knots in a path. It is either
  {ul {- [JLine] is a straight line}
      {- [JCurve] is a spline curve}
      {- [JCurveNoInflex] avoids inflection}
      {- [JTension] permits "tension" on the joint; [JCurve] uses a default
                  tension of 1. Higher tension means less "wild" curves}
      {- [JControls] permits to give control points by hand}} *)
type joint = PrimPath.joint =
  | JLine
  | JCurve
  | JCurveNoInflex
  | JTension of float * float
  | JControls of Point.t * Point.t

(** the abstract type of paths *)
type t = PrimPath.t

(** {2 labelled path constructors} *)
(** build a knot from a pair of floats 
    @param l an incoming direction
    @param r an outgoing direction *)
val knot :
    ?l:PrimPath.direction -> ?r:PrimPath.direction -> 
      ?scale:(float -> Num.t) -> float * float -> PrimPath.knot

(** build a knot from a point; the optional arguments are as in {!knot} *)
val knotp :
    ?l:PrimPath.direction -> ?r:PrimPath.direction -> Point.t -> PrimPath.knot

(** build a path from a list of floatpairs
  @param style the joint style used for all joints of the path
  @param cycle if given, the path is closed using the given style
  @param scale permits to scale the whole path *)
val path : 
    ?style:PrimPath.joint -> ?cycle:PrimPath.joint -> ?scale:(float -> Num.t) -> 
      (float * float) list -> PrimPath.t

(** same as [path], but uses a knot list *)
val pathk :
    ?style:PrimPath.joint -> ?cycle:PrimPath.joint -> PrimPath.knot list -> PrimPath.t

(** same as [path] but uses a point list *)
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

(** close a path using direction [dir] and style [style] *)
val cycle : ?dir:PrimPath.direction -> ?style:PrimPath.joint -> PrimPath.t -> PrimPath.t


(** {2 Primitive path constructors} *)
(** add a knot at the end of a path  *)
val concat : ?style:PrimPath.joint -> PrimPath.t -> PrimPath.knot -> PrimPath.t

(** lift a knot to a path *)
val start : knot -> t

(** append a path to another using joint [style] *)
val append : ?style:PrimPath.joint -> t -> t -> t


(** {2 More complex constructions on paths} *)

(** [point f p] returns a certain point on the path [p]; [f] is given 
    "in control points":
    [1.] means the first control point, [2.] the second and so on; intermediate values 
    are accepted. *)

val point : float -> t -> Point.t

(** [subpath start end path] selects the subpath of [path] that lies between
    [start] and [end]. [start] and [end] are given in control points, as in {!point}. *)
  
  val subpath : float -> float -> t -> t

(** apply a transformation to a path *)
val transform : Transform.t -> t -> t

(** get the bounding path of a box *)
val bpath : Box.t -> t

(** [cut_after p1 p2] cuts [p2] after the intersection with [p1]. To memorize
  the order of the arguments, you can read: "cut after [p1]" *)
val cut_after : t -> t -> t

(** same as {!cut_after}, but cuts before *)
val cut_before: t -> t -> t

(** builds a cycle from a set of intersecting paths *)
val build_cycle : t list -> t


(** {2 Predefined values} *)

val defaultjoint : joint
val fullcircle : t
val halfcircle : t
val quartercircle: t
val unitsquare: t

