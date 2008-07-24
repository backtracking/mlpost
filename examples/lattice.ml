open Mlpost
open Metapost
open Command
open Picture
open Path
open Num
open Num.Infix
open Helpers

let dx = bp 4.
let dy = bp 6.

let node s = make (draw_box (Box.ellipse Point.origin (Picture.tex s)))

let draw_boxes = List.map (draw_box ~boxed:false)

let fig =
  let l1 = Box.halign ~dx (List.map node ["ab"; "ac"; "bc"]) in
  let l2 = Box.halign ~dx (List.map node ["a"; "b"; "c"]) in
  let pl = List.map (fun l -> make (seq (draw_boxes l))) [l1; l2] in
  draw_boxes (Box.valign ~dy pl)
