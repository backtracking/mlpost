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
