open Path
open Point
open Helpers
open Mlpost
module C = Convenience

let draw1 = 1, [ C.draw ~style:JLine [20.,20.; 0.,0.; 0.,30.; 30.,0.; 0.,0.] ]

let z0 = 0.,0.
let z1 = 60.,40.
let z2 = 40.,90.
let z3 = 10.,70.
let z4 = 30.,50.
let l1 = z0::z1::z2::z3::z4::[]

let labels1 =
   (dotlabels ~pos:PTop ["0";"2";"4"] (map_bp [z0;z2;z4])) @
   [dotlabel ~pos:PLeft (tex "3") (bpp z3);
    dotlabel ~pos:PRight (tex "1") (bpp z1) ]

let draw3 = 3, 
  [ C.draw ~style:JCurve  l1 ] @ labels1

let draw4a, draw4b = 
  let labels = dotlabels ~pos:PTop ["2";"4"] (map_bp [z2;z4]) @
               dotlabels ~pos:PLeft ["0";"3"] (map_bp [z0;z3]) @
               [dotlabel ~pos:LowRight (tex "1") (bpp z1)]
  in
    (104, [ C.draw ~cycle:JCurve l1] @ labels) ,
    (204, 
     [ draw 
         (append (C.path [z0;z1;z2;z3]) JLine (C.path ~style:JLine [z4;z0]) )
     ] @ labels)

(* no easy alternative way to draw this one, and that's fine *)
let l1dirs = List.map (fun p -> NoDir,p,NoDir) (map_bp l1)
let lcontrols =
  [(26.8, -1.8), (51.4,14.6);
   (67.1, 61.), (59.8,84.6);
   (25.4, 94.), (10.5,84.5);
   (9.6, 58.8), (18.8,49.6)]
let lcontrolsbp = List.map (fun (a,b) -> JControls (bpp a, bpp b)) lcontrols

let draw5 = 5,
  [ draw (C.jointpath l1 lcontrolsbp) ;
    let hull = 
      List.fold_left2 (fun acc (c1, c2) f -> f::c2::c1::acc) 
	[0.,0.] lcontrols (List.tl l1)
    in
      (* As long as we dont have the dashed lines : gray *)
    let gray = Color.make (Color.Gray 0.5)
    in
      C.draw ~color:gray ~style:JLine (List.rev hull) ] @ labels1

let draw6 = 6, 
  [ draw (path_fold JCurve 
           [ C.p z0; C.p ~r:(Vec up) z1; 
             C.p ~r:(Vec left) z2; C.p z3; C.p z4] ) ] @ labels1


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

let z0 = (-1., 0.)
let z1 = (0., 0.2)
let z2 = ( 1., 0.)
let labels9 = dotlabels ~pos:PBot ["0";"1";"2"] (map_in [z0;z1;z2])
let z0 = C.p ~r:(Vec up) ~scale:C.IN z0
let z1 = C.p ~r:(Vec right) ~scale:C.IN z1
let z2 = C.p ~r:(Vec down) ~scale:C.IN z2

let draw9a = 109, [draw (path_fold JCurve [z0;z1;z2])] @ labels9
let draw9b = 209, [draw (path_fold JCurveNoInflex [z0;z1;z2])] @labels9

let u l = 1.5 /. 10. *. l
let z0 = (u (-5.)), 0.
let z1 = (u (-3.)),u 2.
let z2 = (u 3.),u 2.
let z3 = (u 5.),u 0.
let l1 = [z0;z1;z2;z3]
let labels10 = dotlabels ~pos:PBot ["0";"1";"2";"3"] (map_in l1)

let draw10a = 110, [C.draw ~scale:C.IN l1 ] @ labels10

let draw10b = 210, 
  [ draw (C.jointpath ~scale:C.IN l1 [JCurve; JTension(1.3,1.3); JCurve] ) ] 
  @ labels10
let draw10c = 310, 
  [ draw (C.jointpath ~scale:C.IN l1 [JCurve; JTension(1.5,1.0); JCurve] ) ]
  @ labels10

let u l = 1.4 /. 10. *. l
let z0 = u (2.), u (-5.)
let z1 = 0., 0.
let z2 = u 2., u 5.
let cl = [0.; 1.; 2.; infinity]

let u l = 1.4 /. 10. *. l
let z0 = u (2.), u (-5.)
let z1 = 0., 0.
let z2 = u 2., u 5.
let cl = [0.; 1.; 2.; infinity]
let pat c = [ C.p ~r:(Curl c) ~scale:C.IN z0 ; 
              C.p ~scale:C.IN z1; 
              C.p ~l:(Curl c) ~scale:C.IN z2 ]

let draw11 =
  let numbers = [111; 211; 311; 411] in
  let labels11 = dotlabels ~pos:PRight ["0";"1";"2"] (map_in [z0;z1;z2]) in
    List.map2
      (fun c n -> n,
         [draw
           (path_fold JCurve (pat c) ) ] @ labels11 )
      cl numbers

let figs = 
  [ draw1; draw3; draw4a; draw4b;draw5; 
    draw6; draw7; draw8; draw9a; draw9b;
    draw10a; draw10b; draw10c] @ draw11

let mpostfile = "test/testmanual.mp"
let texfile = "test/testmanual.tex"

let _ =
    Generate.generate_mp mpostfile figs;
    Generate.generate_tex texfile "manual/manual" "testmanual" figs


