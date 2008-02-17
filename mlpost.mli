type command
type t = command list

val draw : ?color:Style.Color.t -> Path.t -> command
val print : int -> Format.formatter -> t -> unit
val iter : int -> int -> (int -> command) -> command

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
