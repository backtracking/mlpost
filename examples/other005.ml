open Command
open Path

let fig =
  let a = 0., 0. in
  let b = 1., 0. in
  let c = 0., 1. in
  let pen = (Pen.circle ~tr:[Transform.scaled (Num.bp 4.)] ()) in
  [ draw (path ~style:JLine ~scale:Num.cm ~cycle:JLine [a;b;c]) ] @
    List.map (fun a -> draw ~pen (path ~scale:Num.cm [a])) [a;b;c]
