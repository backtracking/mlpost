open Mlpost
open Path
module SP = SimplePath

let fig =
  let pen = Pen.transform [Transform.scaled 2.] Pen.circle in
  let triangle = 
    SP.path ~scale:Num.cm ~style:JLine ~cycle:JLine [(0.,0.);(1.,0.);(0.,1.)]
  in
    [Command.fill ~color:(Color.gray 0.8) triangle;
     Command.draw ~pen triangle]
