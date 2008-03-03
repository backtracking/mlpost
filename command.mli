type t

val draw : ?color:Color.t -> ?pen:Pen.t -> Path.t -> t
val fill : ?color:Color.t -> Path.t -> t
val iter : int -> int -> (int -> t list) -> t

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

val label : ?pos:position -> picture -> Point.t -> t
val dotlabel : ?pos:position -> picture -> Point.t -> t

type figure = t list
val print : int -> Format.formatter -> figure -> unit

