open Path

(* generic functions that proved to be useful *)
(* might move into the interface to obtain better metapost *)
let bpp (a,b) = (bp a, bp b)
let map_bp = List.map bpp

(* construct the value f (f (... f(f acc st ) (st + 1) ...) (en -1)) en *)
let rec fold_from_to f st en acc =
  if st <= en then
    fold_from_to f (st+1) en (f acc st)
  else
    acc

(* map [st; ...; en] to [ f st; ...; f en] *)
let map_from_to f st en =
  fold_from_to (fun acc i -> (f i)::acc ) st en []

let path_fold style l =
  match l with
    | [] -> failwith "empty path is not allowed"
    | (x::xs) ->
        List.fold_left (fun p knot -> concat p style knot) (start x) xs

(* ==================== *)

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
      
let draw6 =
  [ draw 
      (path_fold JCurve
	 [NoDir,bpp z0,NoDir;
          NoDir,bpp z1,Vec up;
	  NoDir,bpp z2,Vec left;
	  NoDir,bpp z3,NoDir;
	  NoDir,bpp z4,NoDir])
  ]


let lex = (NoDir,bpp (0.0,0.0),Vec(dir 45.))
let rex a = (Vec(dir (10.*.a)), (cm 6., bp 0.), NoDir)
let draw7 =
  map_from_to 
    (fun a ->
       draw (concat (start lex) JCurve 
	       (rex (float_of_int (-a))))) 0 9

let draw8 =
  map_from_to
    (fun a ->
       draw (concat (start lex) JCurve 
	       (rex (float_of_int a)))) 0 9

let z0 = inch (-1.), bp 0.
let z1 = bp 0., inch 2.
let z2 = inch 1., bp 0.
let draw9a =
  [ draw (path_fold JCurve
      [NoDir, z0, Vec down; NoDir, z1, Vec right;
       NoDir, z2, Vec down] ) ]

let draw9b =
  [ draw (path_fold JCurveNoInflex
      [NoDir, z0, Vec down; NoDir, z1, Vec right;
       NoDir, z2, Vec down] ) ]

let z0 = -5., 0.
let z1 = -3., 2.
let z2 = 3., 2.
let z3 = 5., 0.
let l1 = [z0;z1;z2;z3]

let draw10a = [ Convenience.draw l1 ]
let draw10b = 
  [ draw 
      (concat
         (concat 
            (concat (start (NoDir, bpp z0, NoDir)) JCurve (NoDir, bpp z1, NoDir)) 
            (JTension(1.3,1.3)) (NoDir, bpp z2, NoDir))
         JCurve (NoDir, bpp z3, NoDir)) ]

let draw10b' =
  let jl = [JCurve; JTension(1.3,1.3); JCurve] in
    [ draw
        (List.fold_left2
           (fun acc s p -> concat acc s (NoDir,bpp p,NoDir)) 
           (start (NoDir, bpp z0, NoDir)) jl [z1;z2;z3]) ]

let draw10c =
  let jl = [JCurve; JTension(1.5,1.0); JCurve] in
    [ draw
        (List.fold_left2
           (fun acc s p -> concat acc s (NoDir,bpp p,NoDir)) 
           (start (NoDir, bpp z0, NoDir)) jl [z1;z2;z3]) ]
