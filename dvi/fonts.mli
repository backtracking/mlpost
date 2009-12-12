type font_def = {
  checksum : int32;
  scale_factor : int32;
  design_size : int32;
  area : string;
  name : string;
}


type t = 
    { tex_name : string;
      metric : Tfm.t;
      glyphs_filename : string; (* the file, pfb or pfa, which define the glyphs *)
      glyphs_enc : int -> int; (* the conversion of the characters 
                                  between tex and the font *)
      slant : float option;
      extend : float option;
      ratio : float;
      ratio_cm : float
    }

val set_verbosity : bool -> unit

val load_font : font_def -> float -> t

val t1disasm : string option ref

module Print : sig
  val font : int32 -> Format.formatter -> font_def -> unit
end

val char_width : t -> int -> float
