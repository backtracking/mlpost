open Box
open Tree

(*parse <<togglescript>> *)

(*parse <<tree1 *)
let tree1 =
  let node s = node (tex s) in
  let leaf s = leaf (tex s) in
  [draw (node "1" [node "2" [leaf "4"; leaf "5"]; 
		   node "3" [leaf "6"; leaf "7"]])]
(*parse >> <<tree2 *)
let tree2 =
  let leaf s = leaf  (tex ~style:Rect s) in
  let node s = node ~arrow_style:Undirected (tex ~style:Rect s) in
  [draw 
     (node "1" [node "2" [leaf "4"; leaf "5"]; 
		node "3" [leaf "6"; leaf "7"]])]

(*parse >> <<tree3 *)
let tree3 =
  let node s = node  ~arrow_style:Undirected ~edge_style:Curve (tex s) in
  let leaf s = leaf (tex s) in
  [draw     (node "1" [node "2" [leaf "4"; leaf "5"]; 
		node "3" [leaf "6"; leaf "7"]])]

(*parse >> <<tree4 *)
let tree4 =
  let node s = node ~arrow_style:Undirected ~edge_style:Square (tex s) in
  let leaf s = leaf (tex s) in
  [draw 
     (node "1" [node "2" [leaf "4"; leaf "5"]; 
		node "3" [leaf "6"; leaf "7"]])]

(*parse >> <<tree5 *)
let tree5 =
  let node s = node  ~arrow_style:Undirected ~edge_style:HalfSquare (tex s) in
  let leaf s = leaf (tex s) in
  [draw     
    (node "1" [node "2" [leaf "4"; leaf "5"]; 
		node "3" [leaf "6"; leaf "7"]])]

(*parse >> <<tree6 *)
let tree6 =
  let node s = node ~edge_style:HalfSquare (tex s) in
  let leaf s = leaf (tex s) in
  [draw 
     (node "1" [node "2" [leaf "4"; leaf "5"]; 
		node "3" [leaf "6"; leaf "7"]])]
(*parse >> *)

let _ = 
  List.iter (fun (name,fig) -> Metapost.emit name fig)
  [ "tree1", tree1;
    "tree2", tree2;
    "tree3", tree3;
    "tree4", tree4;
    "tree5", tree5;
    "tree6", tree6;
  ]
