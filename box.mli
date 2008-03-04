
type t

val circle : Point.t -> Picture.t -> t

val center : t -> Point.t

val declare : Format.formatter -> t -> unit

val name : t -> string
