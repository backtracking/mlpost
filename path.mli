(* there might be a Num module later *)
type num
val bp : float -> num
val pt : float -> num
val cm : float -> num
val mm : float -> num
val inch : float -> num

type point
val p : num * num -> point
(* These functions create points of "unspecified" size, ie vectors
   to use with Vec for instance *)
val dir : float -> point
val up : point
val down : point
val left : point
val right : point

(* these are the functions actually related to paths *)
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

val cycle : joint -> t -> t
val append : t -> joint -> t -> t

(* later in the mlpost module *)
open Style

type command
type figure = command list

val draw : ?color:Color.t -> t -> command
val print_fig : int -> Format.formatter -> figure -> unit

type position =
  | Center
  | PLeft
  | PRight
  | PTop
  | PBot
  | UpLeft
  | UpRight
  | LowLeft
  | LowRight

type picture
val tex : string -> picture

val label : ?pos:position -> picture -> point -> command
val dotlabel : ?pos:position -> picture -> point -> command
