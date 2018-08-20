
open Cairo_ft

external ft_get_name_index : ft_face -> string -> int = "ml_FT_Get_Name_Index"


external ft_get_char_index : ft_face -> int -> int = "ml_FT_Get_Char_Index"

external ft_num_charmaps : ft_face -> int = "ml_FT_num_charmaps"

external ft_set_charmap : ft_face -> int -> int = "ml_FT_set_charmap"

let ft_set_charmap face index =
  if index < 0 ||  ft_num_charmaps face <= index then
    invalid_arg "ft_set_charmap : invalid charmap index";
  let r = ft_set_charmap face index in
  if r <> 0 then invalid_arg "ft_set_charmap : unsuccesful"
