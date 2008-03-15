open Mlpost
open SimplePoint
open Command
open Dash
module SP = Path

let fig = 
  let a = cmp (0., 0.) in
  let b = cmp (1., 0.) in
  let c = cmp (0., 1.) in
    [ draw (SP.pathp [a;b]);
      draw ~dashed:evenly (SP.pathp [b;c]);
      draw ~dashed:withdots (SP.pathp [c;a]) ]
