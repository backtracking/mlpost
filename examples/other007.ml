open Mlpost
open Path
open SimplePoint
open Command
module SP = SimplePath

let fig = 
  let a = cmp (0., 0.) in
  let b = cmp (1., 0.) in
  let c = cmp (0., 1.) in
    [draw (SP.pathp ~style:JLine ~cycle:JLine [a;b;c]) ;
     draw (SP.pathp [ Point.segment 0.5 a b ; c]) ;
     draw (SP.pathp [ Point.segment 0.5 b c ; a]) ;
     draw (SP.pathp [ Point.segment 0.5 c a ; b]) ; ]
