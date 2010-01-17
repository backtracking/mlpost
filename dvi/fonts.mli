type font_def 

val mk_font_def : 
  checksum : int32 ->
  scale_factor : int32 ->
  design_size : int32 ->
  area: string -> 
  name:string -> font_def

type t 

val set_verbosity : bool -> unit

val load_font : font_def -> float -> t

val metric : t -> Tfm.t
val tex_name : t -> string
val glyphs_filename : t -> string
(* the file, pfb or pfa, which defines the glyphs *)
val glyphs_enc : t -> (int -> int)
(* the conversion of the characters between tex and the font *)

val ratio_cm : t -> float
val slant : t -> float option
val extend : t -> float option

val t1disasm : string option ref

module Print : sig
  val font : int32 -> Format.formatter -> font_def -> unit
end

val char_width : t -> int -> float
val char_height : t -> int -> float
val char_depth : t -> int -> float

val char_dims : t -> int -> float * float * float
(** width, height, depth of the [i]th char *)

val scale : t -> float -> float
(** [scale t f] scale the given float [f] by [ratio_cm t]  *)
