open Mlpost
open Command
open SimplePath
module T = Transform

let fig =
  let p = path ~cycle:JCurve [(0.,0.); (30.,40.); (40.,-20.); (10.,20.)] in
  let pen = Pen.circle ~tr:[T.scaled 1.5] () in
  [Command.draw p;
   Command.seq 
     (List.map
	 (fun (pos, l, i) -> 
	   Command.dotlabel ~pos (Picture.tex l) (Path.point i p))
	 [Pbot, "0", 0.;  Pupleft, "1", 1. ;
	  Plowleft, "2", 2. ;  Ptop, "3", 3. ; Pleft, "4", 4. ]);
   Command.draw ~pen (subpath 1.3 3.2 p)]

