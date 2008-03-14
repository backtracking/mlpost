
type t = Types.dash

val evenly : t
val withdots : t

val scaled : float -> t -> t
val shifted : Point.t -> t -> t

type on_off = Types.on_off = On of Num.t | Off of Num.t

val pattern : on_off list -> t

