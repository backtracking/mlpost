open Mlpost
module C = Convenience
open Path

let fig = 
  let l = [0.,0.; 1., 0.; 0., 1.] in
    [ C.draw ~style:JLine ~scale:Num.cm ~cycle:JLine l  ]
