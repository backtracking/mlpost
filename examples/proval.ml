open Mlpost
open Command
open Path
module T = Transform
open Num

let fig =
  let k = 7. in
  let pen = Pen.square ~tr:[T.yscaled (f 0.5); T.rotated 40.] () in
  let check = 
    jointpath [-1.2,1.2; 0., -2. ; 2., 2. ; 5., 5.] [JLine ; JCurve; JCurve]
  in
  [fill ~color:Color.black 
      (Path.scale k Path.fullcircle) ;
   label ~pos:Pleft (Picture.tex "Pr") (Point.p (k /. (-4.),0.)) ;
   label ~pos:Pright (Picture.tex "al") (Point.p (k /. 4.,0.)) ;
   draw ~color:Color.green ~pen check;]
