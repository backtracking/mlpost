type t'

val scaled : Num.t -> t'
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
(*
type 'a with_transform
val transform : t -> 'a -> 'a with_transform
val modify : t -> 'a with_transform -> 'a with_transform
val print_with_transform : 
   (Format.formatter -> 'a -> unit) -> 
                         Format.formatter -> 'a with_transform -> unit
                         *)
