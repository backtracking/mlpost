open Mlpost
open Path
open Command

let fig = 
  let a = (0., 0.) in
  let b = (1., 0.) in
  let c = (0., 1.) in
  let pen = Pen.circle ~tr:[Transform.scaled 2.] () in
  let cl = List.map Color.gray [0.8;0.6;0.4] in
    List.map2
      (fun (a,b) color ->
         draw ~pen ~color (path ~style:JLine ~scale:Num.cm [a;b]))
      [a,b;b,c;c,a] cl
