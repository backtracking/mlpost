open Mlpost
open Path
open SimplePoint
open Command
module SP = SimplePath

let fig = 
  let a = cmp (0., 0.) in
  let b = cmp (1., 0.) in
  let c = cmp (0., 1.) in
  let pen = Pen.transform [Transform.scaled 2.] Pen.circle in
    [ draw_arrow (SP.pathp ~style:JLine [c;b;a]);
      draw_arrow ~pen (SP.pathp [a;c]) ; ]
