module Color : sig
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
end


type style

val mk_style : Color.t -> style
val print_style : Format.formatter -> style -> unit
