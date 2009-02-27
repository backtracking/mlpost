type encoding = 
  [ `File of string
  | `Adobe_remap
  | `None]

type font_map = 
    { tex_name : string;
      human_name : string;
      enc_name : string option;
      pfab_name : string;
      slant : float option;
      extend : float option;
    }

