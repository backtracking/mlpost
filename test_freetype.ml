open Cairo_ft
open Mlpost_ft
open Format

let () =
  let filename = Sys.argv.(1) in
  let char_name = Sys.argv.(2) in
  let ft = Cairo_ft.init_freetype () in
  let face = Cairo_ft.new_face ft filename in
  printf "Index of %s : %i" char_name (ft_get_name_index face char_name)

