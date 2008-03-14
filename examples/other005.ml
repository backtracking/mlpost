open Mlpost
module C = Convenience
open Path

let fig =
  let a = 0., 0. in
  let b = 1., 0. in
  let c = 0., 1. in
  let pen = (Pen.circle ~tr:[Transform.scaled 4.] ()) in
  [ C.draw ~style:JLine ~scale:Num.cm ~cycle:JLine [a;b;c] ] @
    List.map (fun a -> C.draw ~pen ~scale:Num.cm [a]) [a;b;c]
