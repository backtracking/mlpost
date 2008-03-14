(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Johannes Kanig, Stephane Lescuyer                       *)
(*  and Jean-Christophe Filliatre                                         *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2, with the special exception on linking              *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

open Mlpost
open Path
open Point
open Command
open Picture
open SimplePoint
module H = Helpers
module SP = SimplePath
module N = Num
module C = Convenience

let draw1 = 1, [ C.draw ~style:JLine [20.,20.; 0.,0.; 0.,30.; 30.,0.; 0.,0.] ]

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

let draw3 = 3, [ C.draw ~style:JCurve  l1 ] @ labels1

let draw4a, draw4b = 
  let labels = H.dotlabels ~pos:Ptop ["2";"4"] (map_bp [z2;z4]) @
               H.dotlabels ~pos:Pleft ["0";"3"] (map_bp [z0;z3]) @
               [dotlabel ~pos:Plowright (tex "1") (bpp z1)]
  in
    (104, [ C.draw ~cycle:JCurve l1] @ labels) ,
    (204, 
     [ draw 
         (Path.append (SP.path [z0;z1;z2;z3]) JLine (SP.path ~style:JLine [z4;z0]) )
     ] @ labels)

(* no easy alternative way to draw this one, and that's fine *)
let l1dirs = List.map (SP.knot ~scale:N.bp) l1
let lcontrols =
  [(26.8, -1.8), (51.4,14.6);
   (67.1, 61.), (59.8,84.6);
   (25.4, 94.), (10.5,84.5);
   (9.6, 58.8), (18.8,49.6)]
let lcontrolsbp = List.map (fun (a,b) -> JControls (bpp a, bpp b)) lcontrols

let draw5 = 5,
  [ draw (SP.jointpath l1 lcontrolsbp) ;
    let hull = 
      List.fold_left2 (fun acc (c1, c2) f -> f::c2::c1::acc) 
	[0.,0.] lcontrols (List.tl l1)
    in
      (* As long as we dont have the dashed lines : gray *)
      C.draw ~color:(Color.gray 0.5) ~style:JLine (List.rev hull) ] @ labels1

let draw6 = 6, 
  [ draw (SP.pathk
           [ SP.knot z0; SP.knot ~r:(Vec up) z1; 
             SP.knot ~r:(Vec left) z2; SP.knot z3; SP.knot z4] ) ] @ labels1


let lex = SP.knot ~r:(Vec(dir 45.)) (0.,0.)
let rex a = SP.knot ~l:(Vec(dir (10.*.a))) ~scale:N.cm (6., 0.)
let draw7 = 7, 
            [Command.iter 0 9
               (fun a ->
                  [draw (concat (start lex) JCurve 
                          (rex (float_of_int (-a))))]) ]

let draw8 = 8,
            [Command.iter 0 7
               (fun a ->
                  [draw (concat (start lex) JCurve 
                          (rex (float_of_int a)))]) ]

let z0 = (-1., 0.)
let z1 = (0., 0.2)
let z2 = ( 1., 0.)
let labels9 = H.dotlabels ~pos:Pbot ["0";"1";"2"] (map_in [z0;z1;z2])
let z0 = SP.knot ~r:(Vec up) ~scale:N.inch z0
let z1 = SP.knot ~r:(Vec right) ~scale:N.inch z1
let z2 = SP.knot ~r:(Vec down) ~scale:N.inch z2

let draw9a = 109, [draw (SP.pathk [z0;z1;z2])] @ labels9
let draw9b = 209, [draw (SP.pathk ~style:JCurveNoInflex [z0;z1;z2])] @labels9

let u l = 1.5 /. 10. *. l
let z0 = (u (-5.)), 0.
let z1 = (u (-3.)),u 2.
let z2 = (u 3.),u 2.
let z3 = (u 5.),u 0.
let l1 = [z0;z1;z2;z3]
let labels10 = H.dotlabels ~pos:Pbot ["0";"1";"2";"3"] (map_in l1)

let draw10a = 110, [C.draw ~scale:N.inch l1 ] @ labels10

let draw10b = 210, 
  [ draw (SP.jointpath ~scale:N.inch l1 [JCurve; JTension(1.3,1.3); JCurve] ) ] 
  @ labels10
let draw10c = 310, 
  [ draw (SP.jointpath ~scale:N.inch l1 [JCurve; JTension(1.5,1.0); JCurve] ) ]
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
let pat c = [ SP.knot ~r:(Curl c) ~scale:N.inch z0 ; 
              SP.knot ~scale:N.inch z1; 
              SP.knot ~l:(Curl c) ~scale:N.inch z2 ]

let draw11 =
  let numbers = [111; 211; 311; 411] in
  let labels11 = H.dotlabels ~pos:Pright ["0";"1";"2"] (map_in [z0;z1;z2]) in
    List.map2
      (fun c n -> n,
         [draw
           (SP.pathk (pat c) ) ] @ labels11 )
      cl numbers

(** Cette version de draw21 est assez cool mais ne marche pas car la largeur du trait
    est scalée avec la figure... *)
(* let draw21 = *)
(*   let path = transform t halfcircle in *)
(*   let r = Vec (p (Num.bp (-.1.), Num.bp (-.2.))) in *)
(*   let fillp =  *)
(*     cycle (Vec up) JCurve (concat path JCurve (C.p ~r ~scale:C.CM (0.,0.))) in *)
(*     21, [fill fillp; draw (transform t fullcircle)] *)
let draw21 = 
  let mp d pt = SP.knot ~r:(Vec d) ~scale:N.cm pt in
  let kl = [mp down (-1.,0.); mp right (0.,-1.); mp up (1.,0.)] in
  let path = SP.pathk kl in
  let r = Vec (point (-.1., -.2.)) in
  let fillp = SP.cycle ~dir:(Vec up)
      (SP.concat path (SP.knot ~r ~scale:N.cm (0.,0.))) in
  let fullp = SP.cycle (SP.concat path (mp left (0.,1.))) in
    21, [fill fillp; draw fullp]

let figs = 
  [ draw1; draw3; draw4a; draw4b;draw5; 
    draw6; draw7; draw8; draw9a; draw9b;
    draw10a; draw10b; draw10c] @ draw11 
  @ [draw21]

let mpostfile = "test/testmanual.mp"
let texfile = "test/testmanual.tex"

let _ =
    Metapost.generate_mp mpostfile figs;
    Generate.generate_tex texfile "manual/manual" "testmanual" figs


