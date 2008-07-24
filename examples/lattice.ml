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

let node s = 
  let s = "\\rule[-0.1em]{0in}{0.8em}" ^ s in
  Box.ellipse Point.origin (Picture.tex s)

let pic b = make (draw_box b)

let draw_boxes = List.map (draw_box ~boxed:false)

let cross_arrows b l = seq (List.map (box_arrow b) l)

let fig =
  let abc = node "abc" in
  let ab = node "ab" and ac = node "ac" and bc = node "bc" in
  let a = node "a" and b = node "b" and c = node "c" in
  let l0 = Box.halign ~dx (List.map pic [abc]) in
  let l1 = Box.halign ~dx (List.map pic [ab; ac; bc]) in
  let l2 = Box.halign ~dx (List.map pic [a; b; c]) in
  let pl = List.map (fun l -> make (seq (draw_boxes l))) [l0; l1; l2] in
  [seq (draw_boxes (Box.valign ~dy pl));
   cross_arrows abc [ab; ac; bc];
   cross_arrows ab [a; b]; cross_arrows ac [a; c]; cross_arrows bc [b; c]]
