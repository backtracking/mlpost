open Mlpost
open SimplePoint
open Path
open Dash
open Transform

let z0 = cmp (0., 0.)
let z1 = cmp (4., 1.)
let cercle = transform [scaled ~scale:Num.cm 1.; shifted z0] fullcircle
let rectangle = transform [shifted z1] 
  (pathp ~style:JLine ~cycle:JLine 
      [mmp (-5., -5.); mmp (5., -5.); mmp (5., 5.); mmp (-5., 5.)])
let p = pathk [NoDir, z0, Vec (dir 150.); NoDir, z1, Vec (dir (-30.))]

let fig = 
  [Command.draw cercle;
   Command.draw rectangle ~dashed:evenly;
   Command.draw p ~dashed:(Dash.scaled 0.3 withdots);
   Command.draw_arrow (cut_before cercle (cut_after rectangle p)) ]
