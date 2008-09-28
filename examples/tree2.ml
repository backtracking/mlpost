open Box
open Tree

let fig =
  let leaf s = leaf  (tex ~style:Rect s) in
  let node s = node  (tex ~style:Rect s) in
  [draw ~arrow_style:Undirected
     (node "1" [node "2" [leaf "4"; leaf "5"]; 
		node "3" [leaf "6"; leaf "7"]])]

