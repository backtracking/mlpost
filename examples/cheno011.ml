open Command
open Path
module T = Transform

let fig =
  let p = path ~cycle:jCurve [(0.,0.); (30.,40.); (40.,-20.); (10.,20.)] in
  let pen = Pen.circle ~tr:[T.scaled (Num.bp 1.5)] () in
  [Command.draw p;
   Command.seq 
     (List.map
	 (fun (pos, l, i) -> 
	   Command.dotlabel ~pos (Picture.tex l) (Path.point i p))
	 [`Bot, "0", 0.;  `Upleft, "1", 1. ;
	  `Lowleft, "2", 2. ;  `Top, "3", 3. ; `Left, "4", 4. ]);
   Command.draw ~pen (subpath 1.3 3.2 p)]

