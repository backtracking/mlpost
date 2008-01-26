open Path
open Helpers
module C = Convenience

let draw1 = C.draw ~style:JLine [20.,20.; 0.,0.; 0.,30.; 30.,0.; 0.,0.]

let z0 = 0.,0.
let z1 = 60.,40.
let z2 = 40.,90.
let z3 = 10.,70.
let z4 = 30.,50.
let l1 = z0::z1::z2::z3::z4::[]
let draw3 = C.draw ~style:JCurve  l1
let draw4a = C.draw ~cycle:true l1
let draw4b = append (C.path l1) JLine (C.path ~style:JLine [z4;z0])

let lcontrols =
  [(26.8, -1.8), (51.4,14.6);
   (67.1, 61.), (59.8,84.6);
   (25.4, 94.), (10.5,84.5);
   (9.6, 58.8), (18.8,49.6)]
let lcontrolsbp = List.map (fun (a,b) -> JControls (bpp a, bpp b)) lcontrols

let draw5 =
  [ draw (C.jointpath l1 lcontrolsbp) ;
    let hull = 
      List.fold_left2 (fun acc (c1, c2) f -> f::c2::c1::acc) 
	[0.,0.] lcontrols (List.tl l1) 
    in
      C.draw ~style:JLine (List.rev hull) ]

let draw6 =
  draw  (path_fold JCurve 
           [ C.p z0; C.p ~r:(Vec up) z1; 
             C.p ~r:(Vec left) z2; C.p z3; C.p z4] )

let lex = C.p ~r:(Vec(dir 45.)) (0.,0.)
let rex a = C.p ~l:(Vec(dir (10.*.a))) ~scale:C.CM (6., 0.)
let draw7 = 7,
  map_from_to 
    (fun a ->
       draw (concat (start lex) JCurve 
	       (rex (float_of_int (-a))))) 0 9

let draw8 = 8,
  map_from_to
    (fun a ->
       draw (concat (start lex) JCurve 
	       (rex (float_of_int a)))) 0 7

let z0 = C.p ~r:(Vec up) ~scale:C.IN (-1., 0.)
let z1 = C.p ~r:(Vec right) ~scale:C.IN (0., 0.2)
let z2 = C.p ~r:(Vec down) ~scale:C.IN (1., 0.)

let draw9a = draw (path_fold JCurve [z0;z1;z2])
let draw9b = draw (path_fold JCurveNoInflex [z0;z1;z2])

let u l = 1.5 /. 10. *. l
let z0 = (u (-5.)), 0.
let z1 = (u (-3.)),u 2.
let z2 = (u 3.),u 2.
let z3 = (u 5.),u 0.
let l1 = [z0;z1;z2;z3]

let draw10a = C.draw ~scale:C.IN l1

let draw10b = C.jointpath ~scale:C.IN l1 [JCurve; JTension(1.3,1.3); JCurve]
let draw10c = C.jointpath ~scale:C.IN l1 [JCurve; JTension(1.5,1.0); JCurve]

let u l = 1.4 /. 10. *. l
let z0 = u (2.), u (-5.)
let z1 = 0., 0.
let z2 = u 2., u 5.
let cl = [0.; 1.; 2.; infinity]
let pat c = [ C.p ~r:(Curl c) ~scale:C.IN z0 ; 
              C.p ~scale:C.IN z1; 
              C.p ~l:(Curl c) ~scale:C.IN z2 ]

let draw11 =
  let labels = [111; 211; 311; 411] in
    List.map2
      (fun c lab -> lab,
         [draw
           (path_fold JCurve (pat c) ) ] )
      cl labels
