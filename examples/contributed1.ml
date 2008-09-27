open Command
open Color
open Box
open Point
open Num

let repository ?wc patches =
  let pbox = circle ~fill:green patches in
  let draw_wbox = match wc with
    | None -> nop
    | Some wc ->
        Box.draw  (circle ~fill:yellow (south pbox) wc)
  in
   [
    draw_wbox;
    Box.draw  pbox;
  ]

let fig = 
  repository ~wc:(Picture.tex "Copie de travail") (Picture.tex "Patches")
