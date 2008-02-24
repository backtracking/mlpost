(* low-level interface *)
(* A path is a succession of knots bound by joints *)
type direction = 
  | Vec of Point.t
  | Curl of float
  | NoDir 

type joint = 
    JLine
  | JCurve
  | JCurveNoInflex
  | JTension of float * float
  | JControls of Point.t * Point.t

type knot = direction * Point.t * direction

type t
val start : knot -> t
val concat : t -> joint -> knot -> t

val cycle : direction -> joint -> t -> t
val append : t -> joint -> t -> t

(* later in the mlpost module *)
val print : Format.formatter -> t -> unit

val fullcircle : t
val halfcircle : t
val quartercircle: t
val unitsquare: t

val transform : Transform.t -> t -> t
