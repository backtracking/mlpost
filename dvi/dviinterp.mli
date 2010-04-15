type color = 
  | RGB of float * float * float
  | CMYK of float * float * float * float
  | HSB of float * float * float
  | Gray of float

type info = {
  color : color
}

val dumb_info : info


module type dev = sig
  type t
  type arg
  type cooked
  val new_document : arg -> Dvi.t -> t
  val new_page : t -> unit
  val fill_rect : t -> info -> float -> float -> float -> float -> unit
    (* fill_rect t x y w h *)
  val draw_char : t -> info -> Fonts.t -> Int32.t -> float -> float -> unit
    (* draw_char t font code x y *)
  val specials : t -> info -> string -> float -> float -> unit
    (* specials t s x y *)
  val end_document : t -> cooked
end

module Interp (Dev  : dev) : sig
  val set_verbosity : bool -> unit
  val load_file : Dev.arg -> string -> Dev.cooked
end
