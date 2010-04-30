(** Load fonts and extract information *)

type font_def
(** stores the information needed to load a font file *)

val mk_font_def :
  checksum : int32 ->
  scale_factor : int32 ->
  design_size : int32 ->
  area: string ->
  name:string -> font_def
(** build an object of type [fontdef] *)

type t
(** the type of a loaded font *)

val set_verbosity : bool -> unit

val load_font : font_def -> float -> t
(** [load_font def f] loads font [def] scaled by [f] *)

val metric : t -> Tfm.t
(** Obtain the font metric *)

val tex_name : t -> string
(** get the name of the font as used by TeX *)

val glyphs_filename : t -> string
(** the file, pfb or pfa, which defines the glyphs *)

val glyphs_enc : t -> (int -> int)
(** the conversion of the characters between tex and the font *)

val ratio_cm : t -> float
(** The font ratio, in cm *)

val slant : t -> float option
(** The font slant, if present *)

val extend : t -> float option
(** The font extent, if present *)

val t1disasm : string option ref

module Print : sig
  val font : int32 -> Format.formatter -> font_def -> unit
end

val char_width : t -> int -> float
val char_height : t -> int -> float
val char_depth : t -> int -> float
(** get information about the [i]th char of the font *)

val char_dims : t -> int -> float * float * float
(** width, height, depth of the [i]th char *)

val scale : t -> float -> float
(** [scale t f] scale the given float [f] by [ratio_cm t]  *)

val design_size : t -> float
(** the design size of the font *)
