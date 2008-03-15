open Mlpost
open SimplePath

let fig =
  let triangle = 
    path ~scale:Num.cm ~style:JLine ~cycle:JLine [(0.,0.);(1.,0.);(0.,1.)]
  in
    [Command.fill ~color:(Color.gray 0.8) triangle;
     Command.draw triangle]
