type file_hdr = {
  lf : int;
  lh : int;
  bc : int;
  ec : int;
  nw : int;
  nh : int;
  nd : int;
  ni : int;
  nl : int;
  nk : int;
  ne : int;
  np : int;
}

type fix_word = float

type header = {
  checksum : int32;
  design_size : fix_word;
  coding_scheme : string option;
  identifier : string option;
  seven_bit_safe_flag : int option;
  face : int option;
}

type char_info_word = {
  width_index : int;
  height_index : int;
  depth_index : int;
  italic_index : int;
  tag : int;
  info_remainder : int
}

type lig_kern_command = {
  skip_byte : int;
  next_char : int;
  op_byte : int;
  kern_remainder : int;
}

type extensible_recipe = {
  top : int;
  mid : int;
  bot : int;
  rep : int;
}

type body = {
  header : header;
  char_info : char_info_word array;
  width : fix_word array;
  height : fix_word array;
  depth : fix_word array;
  italic : fix_word array;
  lig_kern : lig_kern_command array;
  kern : fix_word array;
  exten : extensible_recipe array;
  param : fix_word array;
}

type t = {
  file_hdr : file_hdr;
  body : body
}

val read_file : string -> t

val design_size : t -> float
