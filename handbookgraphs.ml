(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Johannes Kanig, Stephane Lescuyer                       *)
(*  and Jean-Christophe Filliatre                                         *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

open Mlpost
open Command
open Picture
open Path
module H = Helpers
open Num
open Num.Infix
open Point

let draw1 = 1, [ draw 
                   (path ~style:JLine [20.,20.; 0.,0.; 0.,30.; 30.,0.; 0.,0.])]

let z0 = 0.,0.
let z1 = 60.,40.
let z2 = 40.,90.
let z3 = 10.,70.
let z4 = 30.,50.
let l1 = z0::z1::z2::z3::z4::[]

let labels1 =
   (H.dotlabels ~pos:Ptop ["0";"2";"4"] (map_bp [z0;z2;z4])) @
   [dotlabel ~pos:Pleft (tex "3") (bpp z3);
    dotlabel ~pos:Pright (tex "1") (bpp z1) ]

let draw3 = 3, [ draw (path ~style:JCurve  l1) ] @ labels1

let draw4a, draw4b = 
  let labels = H.dotlabels ~pos:Ptop ["2";"4"] (map_bp [z2;z4]) @
               H.dotlabels ~pos:Pleft ["0";"3"] (map_bp [z0;z3]) @
               [dotlabel ~pos:Plowright (tex "1") (bpp z1)]
  in
    (104, [ draw (path ~cycle:JCurve l1)] @ labels) ,
    (204, 
     [ draw 
         (Path.append ~style:JLine (path [z0;z1;z2;z3]) 
            (path ~style:JLine [z4;z0]) )
     ] @ labels)

(* no easy alternative way to draw this one, and that's fine *)
let l1dirs = List.map (knot) l1
let lcontrols =
  [(26.8, -1.8), (51.4,14.6);
   (67.1, 61.), (59.8,84.6);
   (25.4, 94.), (10.5,84.5);
   (9.6, 58.8), (18.8,49.6)]
let lcontrolsbp = List.map (fun (a,b) -> JControls (bpp a, bpp b)) lcontrols

let draw5 = 5,
  [ draw (jointpath l1 lcontrolsbp) ;
    let hull = 
      List.fold_left2 (fun acc (c1, c2) f -> f::c2::c1::acc) 
	[0.,0.] lcontrols (List.tl l1)
    in
      (* As long as we dont have the dashed lines : gray *)
      draw ~dashed:(Dash.scaled 0.5 Dash.evenly) 
        (path ~style:JLine (List.rev hull)) ] @ labels1

let draw6 = 6, 
  [ draw (pathk
           [ knot z0; knot ~r:(Vec up) z1; 
             knot ~r:(Vec left) z2; knot z3; knot z4] ) ] @ labels1


let lex = knot ~r:(Vec(dir 45.)) (0.,0.)
let rex a = knot ~l:(Vec(dir (10.*.a))) ~scale:cm (6., 0.)
let draw7 = 7, 
            [Command.iter 0 9
               (fun a ->
                  draw (concat (start lex) ~style:JCurve 
                          (rex (float_of_int (-a))))) ]

let draw8 = 8,
            [Command.iter 0 7
               (fun a ->
                  draw (concat (start lex) ~style:JCurve 
                          (rex (float_of_int a)))) ]

let z0 = (-1., 0.)
let z1 = (0., 0.2)
let z2 = ( 1., 0.)
let labels9 = H.dotlabels ~pos:Pbot ["0";"1";"2"] (map_in [z0;z1;z2])
let z0 = knot ~r:(Vec up) ~scale:inch z0
let z1 = knot ~r:(Vec right) ~scale:inch z1
let z2 = knot ~r:(Vec down) ~scale:inch z2

let draw9a = 109, [draw (pathk [z0;z1;z2])] @ labels9
let draw9b = 209, [draw (pathk ~style:JCurveNoInflex [z0;z1;z2])] @labels9

let u l = 1.5 /. 10. *. l
let z0 = (u (-5.)), 0.
let z1 = (u (-3.)),u 2.
let z2 = (u 3.),u 2.
let z3 = (u 5.),u 0.
let l1 = [z0;z1;z2;z3]
let labels10 = H.dotlabels ~pos:Pbot ["0";"1";"2";"3"] (map_in l1)

let draw10a = 110, [draw (path ~scale:inch l1) ] @ labels10

let draw10b = 210, 
  [ draw (jointpath ~scale:inch l1 [JCurve; JTension(1.3,1.3); JCurve] ) ] 
  @ labels10
let draw10c = 310, 
  [ draw (jointpath ~scale:inch l1 [JCurve; JTension(1.5,1.0); JCurve] ) ]
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
let pat c = [ knot ~r:(Curl c) ~scale:inch z0 ; 
              knot ~scale:inch z1; 
              knot ~l:(Curl c) ~scale:inch z2 ]

let draw11 =
  let numbers = [111; 211; 311; 411] in
  let labels11 = H.dotlabels ~pos:Pright ["0";"1";"2"] (map_in [z0;z1;z2]) in
    List.map2
      (fun c n -> n,
         [draw
           (pathk (pat c) ) ] @ labels11 )
      cl numbers

let draw17 =
  let a, b = Num.inch (0.7), Num.inch (0.5) in
  let z0 = p (0.,0.) in
  let z1 = pt (a, f 0.) and z3 = pt (f 0. -/ a, f 0.) in
  let z2 = pt (f 0., b) and z4 = pt (f 0., f 0. -/ b) in
    17, [draw (pathp ~cycle:JCurve [z1;z2;z3;z4]);
	 draw (pathp ~style:JLine [z1; z0; z2]);
	 label ~pos:Ptop (tex "a") (segment 0.5 z0 z1);
	 label ~pos:Pleft (tex "b") (segment 0.5 z0 z2);
	 dotlabel ~pos:Pbot (tex "(0,0)") z0
	]

let draw18 =
  let u = Num.cm in
  let pen = Pen.circle ~tr:[Transform.scaled (f 1.)] () in 
  let rec pg = function
    | 0 -> start (knot ~r:(Vec up) ~scale:u (0.,0.))
    | n -> let f = (float_of_int n /. 2.) in 
	concat ~style:JCurve (pg (n-1)) (knot ~scale:u (f, sqrt f)) in
    18, [draw (pathn ~style:JLine [(f 0.,u 2.); (f 0.,f 0.); (u 4.,f 0.)]);
	 draw ~pen (pg 8);
	 label ~pos:Plowright (tex "$ \\sqrt x$") (pt (u 3., u (sqrt 3.)));
	 label ~pos:Pbot (tex "$x$") (pt (u 2., f 0.));
	 label ~pos:Plowleft (tex "$y$") (pt (f 0., u 1.))]
	 
let draw19 =
  let ux, uy = Num.inch 0.01, Num.inch 0.6 in
  let dux, duy = f 120. */ ux, f 4. */ uy in
  let pen = Pen.circle ~tr:[Transform.scaled (f 1.)] () in 
  let axey = Picture.transform [Transform.rotated 90.] (tex "axe $y$") in
  let rec pg = function
    | 0 -> start (knotn ~r:(Vec right) (f 0.,uy))
    | n -> let k = (float_of_int n)*.15. in 
	concat ~style:JCurve (pg (n-1)) 
	  (knotn (f k */ ux, f 2. // (f 1.+/ f (cos (Num.deg2rad k)))*/uy)) in
    19, [draw (pathn ~style:JLine [(f 0.,duy); (f 0.,f 0.); (dux,f 0.)]);
	 draw ~pen (pg 8);
	 label ~pos:Pbot (tex "axe $x$") (pt (f 60.*/ux, f 0.));
	 label ~pos:Pleft axey (pt (f 0., f 2.*/uy));
	 label ~pos:Pleft (tex "$\\displaystyle y={2\\over1+\\cos x}$")
	   (pt (dux, duy))]

(** Cette version de draw21 est assez cool mais ne marche pas car la largeur du trait
    est scalée avec la figure... *)
(* let draw21 = *)
(*   let path = transform t halfcircle in *)
(*   let r = Vec (p (Num.bp (-.1.), Num.bp (-.2.))) in *)
(*   let fillp =  *)
(*     cycle (Vec up) JCurve (concat path JCurve (C.p ~r ~scale:C.CM (0.,0.))) in *)
(*     21, [fill fillp; draw (transform t fullcircle)] *)
let draw21 = 
  let mp d pt = knot ~r:(Vec d) ~scale:cm pt in
  let kl = [mp down (-1.,0.); mp right (0.,-1.); mp up (1.,0.)] in
  let path = pathk kl in
  let r = Vec (p (-.1., -.2.)) in
  let fillp = cycle ~dir:(Vec up)
      (concat path (knot ~r ~scale:cm (0.,0.))) in
  let fullp = cycle (concat path (mp left (0.,1.))) in
    21, [fill fillp; draw fullp]

let draw22 =
  let a = Path.scale (cm 2.) fullcircle in
  let aa = Path.scale (cm 2.) halfcircle in
  let b = Path.shift (pt (f 0., Num.cm 1.)) a in
  let pa = make (label (tex "$A$") (pt (f 0., Num.cm (-0.5)))) in
  let pb= make (label (tex "$B$") (pt (f 0., Num.cm 1.5))) in  
  let ab = build_cycle [aa; b] in
  let pic = make
    (seq [fill ~color:(Color.gray 0.7) a;
	  fill ~color:(Color.gray 0.7) b;
          fill ~color:(Color.gray 0.4) ab;
          fill ~color:Color.white (bbox pa); draw_pic pa;
          fill ~color:Color.white (bbox pb); draw_pic pb;
          label ~pos:Pleft (tex "$U$") (p ~scale:Num.cm (-1.,0.5)); ])
  in
    22, [draw_pic pic; draw (bbox pic)]

let draw40 =
  let k1 = knot ~r:(Curl 0.) ~scale:Num.pt (0.,0.) in
  let k2 = knot ~scale:Num.pt (5., -3.) in
  let k3 = knot ~scale:Num.pt ~l:(Curl 0.) (10.,0.) in
  let p1 = pathk [k1;k2;k3] in
  let p2 = append p1 (Path.shift (p ~scale:Num.pt (10.,0.)) 
                       (Path.yscale (f (-1.)) p1)) in
  let p2 = 
    Misc.fold_from_to
      (fun acc i -> 
        append acc (Path.shift (p ~scale:Num.pt (float_of_int i *. 20.,0.)) p2))
      p2 1 3 in
  let cmd = 
    Command.iter 0 8
      (fun i ->
        draw (Path.shift (p ~scale:Num.pt (0., float_of_int i *. 10.)) p2))
  in
  let pic = Picture.make cmd in
  let pth = 
    Path.scale (Num.pt 72.) (Path.shift (p (0.5, 0.5)) fullcircle) in
  let pic' = Picture.clip pic pth in
    40, [ draw_pic pic'; draw pth]

let figs = 
  [ draw1; draw3; draw4a; draw4b;draw5; 
    draw6; draw7; draw8; draw9a; draw9b;
    draw10a; draw10b; draw10c] @ draw11 
  @ [draw17; draw18; draw21; draw22; draw40]

let mpostfile = "test/testmanual.mp"
let texfile = "test/testmanual.tex"

let _ =
    Metapost.generate_mp mpostfile figs;
    Generate.generate_tex texfile "manual/manual" "testmanual" figs


