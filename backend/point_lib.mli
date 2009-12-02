type t = Ctypes.point = 
  { x : float; y : float }

val zero : t
val add : t -> t -> t
val sub : t -> t -> t
val mult : float -> t -> t
val div : t -> float -> t
val rotated : float -> t -> t
val transform : Ctypes.matrix -> t -> t

val swapmx : t -> t
val swapmy : t -> t

val sign : t -> t

val middle : t -> t -> t

val norm : t -> float

val list_min_max : ('a -> t * t) -> 'a list -> t * t

val opp : t -> t

val print : Format.formatter -> t -> unit

  module Infix :
  sig
    val (+/) : t -> t -> t
    val (-/) : t -> t -> t
    val ( */) : float -> t -> t
    val ( //) : t -> float -> t
  end
