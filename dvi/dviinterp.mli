(** This module provides a functor to read DVI files *)

(** The type of colors in a DVI file *)
type color = 
  | RGB of float * float * float
  | CMYK of float * float * float * float
  | HSB of float * float * float
  | Gray of float

(** The info type *)
type info = {
  color : color
}

val dumb_info : info


module type dev = sig
  type t (** The environment of the device *)

  type arg (** Initialisation argument of the device *)

  type cooked (** The final value of the device *)

  val new_document : arg -> Dvi.t -> t
  (** this function is initially called *)

  val new_page : t -> unit
  (** this function is called for each new page *)

  val fill_rect : t -> info -> float -> float -> float -> float -> unit
  (** [fill_rect t x y w h] should draw a rectangle at [(x,y)] of width [w] and
   * height [h] on device [t]. *)
  val draw_char : t -> info -> Fonts.t -> Int32.t -> float -> float -> unit
  (** [draw_char t info font code x y] should draw glyph [code] of font [font]
      at pos. [(x,y)] on device [t]. [info] can contain additional information
      such as color. *)
  val specials : t -> info -> string -> float -> float -> unit
  (** [specials t info s x y] should draw special [s], encoded as a string,
      at pos. [(x,y)] on device [t]. [info] can contain additional information
      such as color. *)

  val end_document : t -> cooked
  (** this function is called at the end of the document and should return the
     overall return value of this device. *)
end
(** The signature of DVI devices *)

module Interp (Dev  : dev) : sig
  val set_verbosity : bool -> unit
  val load_file : Dev.arg -> string -> Dev.cooked
  (** [load_file arg fn] loads the dvi document in file [fn], passes [arg] and
    the loaded document to {!Dev.new_document} and calls the drawing functions
    of {!Dev} as needed. At the end, the return value of the device is returned.
    *)
end
(** A functor for reading DVI *)
