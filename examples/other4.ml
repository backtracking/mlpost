open Mlpost

let fig =
  [ Convenience.draw ~pen:(Pen.transform [Transform.scaled 4.] Pen.circle) 
      [(0.,0.)] ]
