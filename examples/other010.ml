open Mlpost
open Path
open SimplePoint
open Command

let fig = 
  let a = cmp (0., 0.) in
  let b = cmp (1., 0.) in
  let c = cmp (0., 1.) in
    [draw (pathp ~style:JLine [b;c;a]) ;
     draw (pathp ~style:JLine [a;b]) ~color:Color.yellow; ]
