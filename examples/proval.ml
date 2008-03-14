open Mlpost
open Path
open Command
module SP = SimplePath
module T = Transform

let fig =
  let f = 7. in
  let pen = Pen.transform [T.yscaled 0.5; T.rotated 40.] Pen.square in
  let check = 
    SP.jointpath [-1.2,1.2; 0., -2. ; 2., 2. ; 5., 5.] [JLine ; JCurve; JCurve]
  in
  [fill ~color:Color.black 
      (Path.transform [T.scaled f] Path.fullcircle) ;
   label ~pos:Pleft (Picture.tex "Pr") (Point.p (f /. (-4.),0.)) ;
   label ~pos:Pright (Picture.tex "al") (Point.p (f /. 4.,0.)) ;
   draw ~color:Color.green ~pen check;]
