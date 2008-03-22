open Mlpost
open Path
open Command

let fig =
  [ draw ~pen:(Pen.circle ~tr:[Transform.scaled 4.] ()) 
      (path [(0.,0.)])]
