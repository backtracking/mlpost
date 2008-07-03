open Mlpost
open Path
open Point
open Command

let fig = 
  let a = cmp (0., 0.) in
  let b = cmp (1., 0.) in
  let c = cmp (0., 1.) in
    [draw (pathp ~style:JLine ~cycle:JLine [a;b;c]) ;
     draw (pathp [ Point.segment 0.5 a b ; c]) ;
     draw (pathp [ Point.segment 0.5 b c ; a]) ;
     draw (pathp [ Point.segment 0.5 c a ; b]) ; ]
