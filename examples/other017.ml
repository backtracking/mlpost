open Mlpost
open Path
open Point
open Command

let fig = 
  let a = cmp (0., 0.) in
  let b = cmp (1., 0.) in
  let c = cmp (0., 1.) in
  let pen = Pen.circle ~tr:[Transform.scaled 2.] () in
    [ draw_arrow (pathp ~style:JLine [c;b;a]);
      draw_arrow ~pen (pathp [a;c]) ; ]
