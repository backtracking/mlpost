open Path

let bpp (a,b) = (bp a, bp b)
let map_bp = List.map bpp

let draw1 = 
  [ draw (straight (map_bp [20.,20.; 0.,0.; 0.,30.; 30.,0.; 0.,0.]))]

let draw3 =
  [ draw (curved (map_bp [0.,0.; 60.,40.; 40.,90.; 10., 70.;30.,50.]))]

let z0 = 0.,0.
let z1 = 60.,40.
let z2 = 40.,90.
let z3 = 10.,70.
let z4 = 40.,50.
let l1 = z0::z1::z2::z3::z4::[]

let draw4a = [ draw (cycle Curved (curved (map_bp l1)))]
let draw4a' = [ Convenience.draw ~cycle:true l1]

let draw4b = [ draw 
		 (append 
		    (curved (map_bp [z0;z1;z2;z3]))
		    (straight (map_bp [z3;z4;z0])))]
(* no easy alternative way to draw this one, and that's fine *)
let draw4b' = [ draw (curved (map_bp [z0;z1;z2;z3]));
		draw (straight (map_bp [z3;z4;z0])) ]

let l1dirs = List.map (fun p -> NoDir,p,NoDir) (map_bp l1)
let lcontrols =
  [(26.8, -1.8), (51.4,14.6);
   (67.1, 61.), (59.8,84.6);
   (25.4, 94.), (10.5,84.5);
   (9.6, 58.8), (18.8,49.6)]

let draw5 = 
  [ draw 
      (List.fold_left2 concat (start (List.hd l1dirs))
	 (List.map (fun (p1,p2) -> JControls(bpp p1,bpp p2)) lcontrols)
	 (List.tl l1dirs));
    (** sur ce dessin, il y aussi le tracé "droit" qui suit les points
	de controle, il faudra ajouter les pointillés pour ça. *)
    let hull = 
      List.fold_left2 (fun acc (c1, c2) f -> f::c2::c1::acc) 
	[z0] lcontrols (List.tl l1) in
      Convenience.draw ~style:Straight (List.rev hull)]
      

  

