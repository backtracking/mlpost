open Mlpost
open Num.Infix
open Box
open Command

let n = 10
let r = float n *./ Num.bp 5.

let node i = 
  let theta = float i *. 2. *. Num.pi /. float n in
  let point = Point.pt (cos theta *./ r, sin theta *./ r) in
  Box.circle point (Picture.tex (string_of_int i))

let boxes = Array.init n node

let arrow i j = if i <> j then Helpers.box_line boxes.(i) boxes.(j) else nop

let fig =
  [iter 0 (n-1) (fun i -> Box.draw boxes.(i));
   iter 0 (n-1) (fun i -> iter 0 (n-1) (arrow i))]

