open Mlpost
open Path
module C = Convenience

let fig = 
  let a = (0., 0.) in
  let b = (1., 0.) in
  let c = (0., 1.) in
  let pen = Pen.transform [Transform.scaled 2.] Pen.circle in
  let cl = List.map Color.gray [0.8;0.6;0.4] in
    List.map2
      (fun (a,b) color ->
         C.draw ~style:JLine ~scale:Num.cm ~pen ~color [a;b])
      [a,b;b,c;c,a] cl
