open Path
open Point
open Command

let fig = 
  let a = cmp (0., 0.) in
  let b = cmp (1., 0.) in
  let c = cmp (0., 1.) in
    [draw (pathp ~style:jLine [b;c;a]) ;
     draw (pathp ~style:jLine [a;b]) ~color:Color.yellow; ]
