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

let pic s = 
  let s = "\\rule[-0.1em]{0in}{0.8em}" ^ s in 
  Box.circle Point.origin (Picture.tex s)

let draw_boxes = List.map (Box.draw ~boxed:true)

let cross_arrows b l = seq (List.map (box_arrow b) l)

module Ab = Pos.Align(Box)
module Abl = Pos.Align(Ab)

let fig =
  let abc = pic "abc" in
  let ab = pic "ab" and ac = pic "ac" and bc = pic "bc" in
  let a = pic "a" and b = pic "b" and c = pic "c" in
  let l1 = Ab.horizontal ~dx [abc;] in
  let l2 = Ab.horizontal ~dx [ab;ac;bc;] in
  let l3 = Ab.horizontal ~dx [a;b;c;] in
  let [l1;l2;l3] = Abl.v (Abl.vertical ~dy [l1;l2;l3]) in
  let [abc] = l1 in
  let [ab;ac;bc] = l2 in
  let [a;b;c] = l3 in
  [seq (draw_boxes l1); seq (draw_boxes l2); seq (draw_boxes l3);
   cross_arrows abc l2; 
   cross_arrows ab [a;b]; cross_arrows ac [a;c]; cross_arrows bc [b;c];
   ]

