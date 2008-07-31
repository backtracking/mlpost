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

(***

type node = N of Box.t * node list (* a node and its successors *)
type lattice = node list list (* nodes lines, from top to bottom *)

let dx = bp 4.
let dy = bp 6.

module Ab = Pos.Align(Box)
module Abl = Pos.Align(Ab)
module H = Hashtbl.Make(struct 
  type t = node let hash (N (b,_)) = Hashtbl.hash b let equal = (==) 
end)

let draw_lattice l =
  let nodes = H.create 97 in
  let rec draw = function
    | [] ->
	[]
    | l :: ll ->
	let ll = draw ll in
	let bl = List.map (function N (b,l) -> b) l in
	let bl = Ab.horizontal ~dx bl in
	let map = List.map (H.find nodes) in
	(List.map2 
	    (fun (N (_,l) as n) b' -> 
	      let n' = N (b', map l) in H.add nodes n n'; n') 
	    l (Ab.v bl)) 
	:: ll
  in
  draw l

(* example *)

let node s l = 
  let s = "\\rule[-0.1em]{0in}{0.8em}" ^ s in 
  N (Box.circle Point.origin (Picture.tex s), l)

let draw_boxes = List.map (Box.draw ~boxed:true)

let cross_arrows b l = seq (List.map (box_arrow b) l)

let fig =
  let a = node "a" [] and b = node "b" [] and c = node "c" [] in
  let ab = node "ab" [a;b] and ac = node "ac" [a;c] and bc = node "bc" [b;c] in
  let abc = node "abc" [ab; ac; bc] in
  let l : lattice = [[abc]; [ab; ac; bc]; [a; b; c]] in
  [nop]


***)
