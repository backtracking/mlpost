type t

type spec =
  | RGB of float * float * float
  | Gray of float
      (* O-ary colors ^^ *)
  | Black
  | White
      (* primary colors *)
  | Red
  | Green
  | Blue
      (* secondary colors *)
  | Yellow
  | Cyan
  | Magenta
      (* er.. help me out there ! *)
  | Orange
  | Purple

val default : t
val make : spec -> t

val red : t

val print : Format.formatter -> t -> unit
