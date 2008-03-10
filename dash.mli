
type t

val evenly : t
val evenly_scaled : float -> t
val withdots : t
val withdots_scaled : float -> t

val print : Format.formatter -> t -> unit

