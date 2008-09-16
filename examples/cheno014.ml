open Point
open Path
open Dash
open Transform

let z0 = cmp (0., 0.)
let z1 = cmp (4., 1.)

let cercle = Path.shift z0 (Path.scale (Num.cm 1.) fullcircle)
let rectangle = Path.shift z1
  (path ~style:jLine ~cycle:jLine ~scale:Num.mm
      [-5., -5.; 5., -5.; 5., 5.; -5., 5.])
let p = pathk 
  (knotlist [noDir, z0, vec (dir 150.); noDir, z1, vec (dir (-30.))])

let fig = 
  [Command.draw cercle;
   Command.draw rectangle ~dashed:evenly;
   Command.draw p ~dashed:(Dash.scaled 0.3 withdots);
   Command.draw_arrow (cut_before cercle (cut_after rectangle p)) ]
