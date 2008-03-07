open Mlpost
open Tree

let fig =
  draw (node "A" [node "B" [leaf "F"]; 
		  node "C" [leaf "D"; leaf "E"]])
