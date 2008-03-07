
(* diagrams *)

type node

type node_style = Circle | Rect

val node : 
  ?style:node_style -> ?scale:(float -> Num.t) -> 
  float -> float -> string -> node
  (** default scale is 40bp *)

type dir = Up | Down | Left | Right | Angle of float

type t

val create : node list -> t

val arrow : 
  t -> ?lab:string -> ?pos:Command.position -> 
  ?outd:dir -> ?ind:dir -> node -> node -> unit

val draw : 
  ?fill:Color.t -> ?stroke:Color.t -> ?pen:Pen.t ->
  t -> Command.figure

