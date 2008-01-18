(* there might be a Num module later *)
type num
type point = num * num
val bp : float -> num
val pt : float -> num
val cm : float -> num
val mm : float -> num
val inch : float -> num

(* These functions create points of "unspecified" size, ie vectors
   to use with Vec for instance *)
val dir : float -> point
val up : point
val down : point
val left : point
val right : point

(* these are the functions actually related to paths *)
type style = Straight | Curved
type t

(* low-level interface *)
(* A path is a succession of knots bound by joints *)
type direction = 
  | Vec of point
  | Curl of float
  | NoDir 

type joint = 
    JLine
  | JCurve
  | JCurveNoInflex
  | JTension of float * float
  | JControls of point * point

type knot = direction * point * direction

val start : knot -> t
val concat : t -> joint -> knot -> t

val cycle : style -> t -> t
val append : t -> t -> t

(* later in the mlpost module *)
type command
type figure = command list

val draw : t -> command
