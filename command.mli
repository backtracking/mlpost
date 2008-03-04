type t

val draw : ?color:Color.t -> ?pen:Pen.t -> Path.t -> t
val fill : ?color:Color.t -> Path.t -> t
val iter : int -> int -> (int -> t list) -> t
val draw_box : Box.t -> t

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

val label : ?pos:position -> Picture.t -> Point.t -> t
val dotlabel : ?pos:position -> Picture.t -> Point.t -> t

type figure = t list
val print : int -> Format.formatter -> figure -> unit

