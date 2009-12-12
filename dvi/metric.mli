type t = Tfm.t

(* convenience interface for font metrics *)

val char_width : t -> int -> float
(* [char_width t i] returns the width of the [i]th character of the font metric
   [t], [0] is the first character *)

val char_height : t -> int -> float
(* same as [char_width], but for character height *)
val char_depth : t -> int -> float
(* same as [char_width], but for character depth *)
val char_italic : t -> int -> float
(* same as [char_width], but for italic correction of the character *)
