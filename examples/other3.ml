open Mlpost
module C = Convenience
open Path

let fig =
  let a =  -1. , -1. in
  let b =  1. , -1. in
  let c =  1. , 1. in
  let d =  -1. , 1. in
    [ C.draw ~style:JLine ~scale:Num.cm ~cycle:JLine [a;b;c;d] ;
      C.draw ~scale:Num.cm [a;c];
      C.draw ~scale:Num.cm [b;d];
      ]


