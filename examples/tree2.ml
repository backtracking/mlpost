open Mlpost
open Tree

let fig =
  let leaf = leaf ~style:Rect in
  let node = node ~style:Rect in
  [draw ~arrow_style:Undirected
     (node "1" [node "2" [leaf "4"; leaf "5"]; 
		node "3" [leaf "6"; leaf "7"]])]

