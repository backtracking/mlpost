open Mlpost
open Metapost
open Command
open Picture
open Path
open Num
open Num.Infix
open Helpers

type node = N of Box.t * node list (* a node and its successors *)
type lattice = node list list (* nodes lines, from top to bottom *)

let dx = bp 12.
let dy = bp 12.

module Ab = Pos.List_(Box)
module Abl = Pos.List_(Ab)
module H = Hashtbl.Make(struct 
  type t = Box.t let hash b = Hashtbl.hash b let equal = (==) 
end)

let nodes = H.create 97

let draw la =
  let line l = Ab.horizontal ~dx (List.map (function (N (b,_)) -> b) l) in
  let la' = Abl.v (Abl.vertical ~dy (List.map line la)) in
  List.iter2
    (List.iter2 (fun (N (b, _)) b' -> H.add nodes b b'))
    la la';
  let box b = H.find nodes b in
  let draw_node (N (b,l)) =
    let b = box b in
    Box.draw b ++ iterl (fun (N(s,_)) -> box_arrow b (box s)) l
  in
  iterl (iterl draw_node) la

(* example *)

let node s l = 
  let s = "\\rule[-0.1em]{0in}{0.8em}" ^ s in 
  N (Box.circle Point.origin (Picture.tex s), l)

let draw_boxes = List.map (Box.draw ~boxed:true)

let cross_arrows b l = seq (List.map (box_arrow b) l)

let fig =
  let a = node "a" [] and b = node "b" [] 
  and c = node "c" [] and d = node "d" [] in
  let ab = node "ab" [a;b] and ac = node "ac" [a;c] 
  and ad = node "ad" [a;d] and bc = node "bc" [b;c] 
  and bd = node "bd" [b;d] and cd = node "cd" [c;d] in
  let abc = node "abc" [ab; ac; bc] 
  and abd = node "abd" [ab; ad; bd]
  and acd = node "acd" [ac; ad; cd]
  and bcd = node "bcd" [bc; bd; cd] in
  let abcd = node "abcd" [abc; abd; acd; bcd] in
  let l : lattice = [[abcd]; [abc; abd; acd; bcd]; 
		     [ab; ac; ad; bc; bd; cd]; [a; b; c; d]] in
  [draw l]
