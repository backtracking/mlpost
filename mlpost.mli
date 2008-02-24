type command
type t = command list

val draw : ?color:Color.t -> ?pen:Pen.t -> Path.t -> command
val fill : ?color:Color.t -> Path.t -> command
val iter : int -> int -> (int -> command list) -> command

val print : int -> Format.formatter -> t -> unit

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

val label : ?pos:position -> picture -> Point.t -> command
val dotlabel : ?pos:position -> picture -> Point.t -> command
