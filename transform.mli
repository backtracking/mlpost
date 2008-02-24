type t'

val scaled : ?scale:(float -> Num.t) -> float -> t'
val rotated : float -> t'
val shifted : Point.t -> t'
val slanted : Num.t -> t'
val xscaled : Num.t -> t'
val yscaled : Num.t -> t'
val zscaled : Point.t -> t'
val reflect : Point.t -> Point.t -> t'
val rotate_around : Point.t -> float -> t'

type t = t' list
val id : t

val print : Format.formatter -> t -> unit
