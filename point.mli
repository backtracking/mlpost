module Num : sig
  type t
  val bp : float -> t
  val pt : float -> t
  val cm : float -> t
  val mm : float -> t
  val inch : float -> t
  val print : Format.formatter -> t -> unit
  val print_float : Format.formatter -> float -> unit
end

type t
val p : Num.t * Num.t -> t
(* These functions create points of "unspecified" size, ie vectors
   to use with Vec for instance *)
val dir : float -> t
val up : t
val down : t
val left : t
val right : t
val print : Format.formatter -> t -> unit
