open Mlpost
open Command
open Color
open Box
open Point
open Num

let repository ?wc patches =
  let pbox = circle (p (0.,0.)) patches in
  let draw_wbox = match wc with
    | None -> nop
    | Some wc ->
        draw_box ~fill: yellow (circle (south pbox) wc)
  in
   [
    draw_wbox;
    draw_box ~fill: green pbox;
  ]

let fig = 
  repository ~wc: (Picture.tex "Copie de travail") (Picture.tex "Patches")
