(* taken from
 * http://www.cs.ucc.ie/~dongen/mpost/mp/Escher87.mp
*)
open Mlpost
open Command
open Point
open Path

let rec fold_append ?(style=JCurve) = function
  | [] -> failwith "No empty list allowed"
  | [x] -> x
  | (x::xs) -> append ~style x (fold_append xs)

let s = 0.004 
let xs1 = 48.
let xs2 = 25.
let ys = 19.

let add (a1,a2) (b1,b2) = (a1 +. b1 , a2 +. b2)
let mult f (a1,a2) = (f *. a1, f *. a2) 
let myscale = List.map (mult s)

let fig =
  let pen1 = Pen.circle () in
  let mygreen = Color.rgb 0.8 0.9 0.8 in
  let p1  = ( 750.,8000. -. 4950. ) in
  let p2  = (1050.,8000. -. 4950. ) in
  let p3  = (2100.,8000. -. 4800. ) in
  let p4  = (2925.,8000. -. 4650. ) in
  let p5  = (4050.,8000. -. 5100. ) in
  let p6  = (4050.,8000. -. 5550. ) in
  let p7  = (3750.,8000. -. 6075. ) in
  let p8  = (3150.,8000. -. 6900. ) in
  let p9  = (3075.,8000. -. 7500. ) in
  let p10 = (3525.,8000. -. 7950. ) in
  let p11 = (4275.,8000. -. 8775. ) in
  let p12 = (5400.,8000. -. 9300. ) in
  let p13 = (4725.,8000. -. 8550. ) in
  let p14 = (4275.,8000. -. 7725. ) in
  let p15 = (4875.,8000. -. 8325. ) in
  let p16 = (5550.,8000. -. 8700. ) in
  let p17 = (5100.,8000. -. 7950. ) in
  let p18 = (4800.,8000. -. 7125. ) in
  let p19 = (5400.,8000. -. 7725. ) in
  let p20 = (6150.,8000. -. 8100. ) in
  let p21 = (5550.,8000. -. 7275. ) in
  let p22 = (5250.,8000. -. 6375. ) in
  let p23 = (5850.,8000. -. 7050. ) in
  let p24 = (6600.,8000. -. 7500. ) in
  let p25 = (6075.,8000. -. 6675. ) in
  let p26 = (5700.,8000. -. 5775. ) in
  let p27 = (6975.,8000. -. 7125. ) in
  let p28 = (8625.,8000. -. 7950. ) in
  let p29 = (7875.,8000. -. 7350. ) in
  let p30 = (7275.,8000. -. 6750. ) in
  let p31 = (8175.,8000. -. 7200. ) in
  let p32 = (9150.,8000. -. 7425. ) in
  let p33 = (8325.,8000. -. 6975. ) in
  let p34 = (7725.,8000. -. 6375. ) in
  let p35 = (8550.,8000. -. 6750. ) in
  let p36 = (9525.,8000. -. 6825. ) in
  let p37 = (8625.,8000. -. 6450. ) in
  let p38 = (8100.,8000. -. 6000. ) in
  let p39 = (9000.,8000. -. 6300. ) in
  let p40 = (9975.,8000. -. 6300. ) in
  let p41 = (9075.,8000. -. 6000. ) in
  let p42 = (8400.,8000. -. 5550. ) in
  let p43 = (9525.,8000. -. 5925. ) in
  let p44 = (10425.,8000.-. 5925. ) in
  let p45 = (9300.,8000. -. 5550. ) in
  let p46 = (8250.,8000. -. 5100. ) in
  let p47 = (7275.,8000. -. 4875. ) in
  let p48 = (6300.,8000. -. 4800. ) in
  let p49 = (7275.,8000. -. 4500. ) in
  let p50 = (8400.,8000. -. 4500. ) in
  let p51 = (7500.,8000. -. 4050. ) in
  let p52 = (6825.,8000. -. 3900. ) in
  let p53 = (7800.,8000. -. 3825. ) in
  let p54 = (8700.,8000. -. 3975. ) in
  let p55 = (7875.,8000. -. 3375. ) in
  let p56 = (7050.,8000. -. 3075. ) in
  let p57 = (8175.,8000. -. 3150. ) in
  let p58 = (8925.,8000. -. 3450. ) in
  let p59 = (8175.,8000. -. 2775. ) in
  let p60 = (7350.,8000. -. 2400. ) in
  let p61 = (8250.,8000. -. 2475. ) in
  let p62 = (9225.,8000. -. 3000. ) in
  let p63 = (8850.,8000. -. 2100. ) in
  let p64 = (8400.,8000. -. 1650. ) in
  let p66 = (8100.,8000. -. 1875. ) in
  let p67 = (7200.,8000. -. 1575. ) in
  let p68 = (5850.,8000. -. 1500. ) in
  let p69 = (5625.,8000. -. 2025. ) in
  let p70 = (5475.,8000. -. 2400. ) in
  let p71 = (5100.,8000. -. 3000. ) in
  let p72 = (4650.,8000. -. 3750. ) in
  let p73 = (3525.,8000. -. 3450. ) in
  let p74 = (2550.,8000. -. 3075. ) in
  let p75 = (2325.,8000. -. 3375. ) in
  let p76 = (2100.,8000. -. 3600. ) in
  let p77 = (1425.,8000. -. 4050. ) in
  let p78 = ( 975.,8000. -. 4350. ) in
  let p79 = ( 525.,8000. -. 4875. ) in
  let p80 = (1840.,8000. -. 4600. ) in
  let p81 = (2375.,8000. -. 4550. ) in
  let line1 = path (myscale [p79;p1;p2;p3;p4;p5]) in
  let line2 = 
    fold_append ~style:JLine 
      (List.map (fun l -> path (myscale l) )
      [  [p9;p10;p11;p12] ; [p12; p13; p14] ;
         [p14; p15; p16] ; [p16; p17; p18] ;
         [p18; p19; p20] ; [p20; p21; p22] ;
         [p22; p23; p24] ; [p24; p25; p26] ;
         [p26; p27; p28] ; [p28; p29; p30] ;
         [p30; p31; p32] ; [p32; p33; p34] ;
         [p34; p35; p36] ; [p36; p37; p38] ;
         [p38; p39; p40] ; [p40; p41; p42] ;
         [p42; p43; p44] ; [p44; p45; p46] ;
         [p46; p47; p48] ; [p48; p49; p50] ;
         [p50; p51; p52] ; [p52; p53; p54] ;
         [p54; p55; p56] ; [p56; p57; p58] ;
         [p58; p59; p60] ; [p60; p61; p62] ;
         [p62; p66; p67; p68 ] ]) in
  let line3 = path (myscale  [p62; p63; p64 ]) in
  let line4 = path (myscale [p72; p73; p74 ]) in
  let line5  = path (myscale [p79; p80; p81]) in
  let line6  = path (myscale [p6; p6; p7; p8; p9 ]) in
  let line7  = path (myscale [p74; p75; p76; p77; p78; p78; p79]) in
  let line8  = path (myscale [p68; p69; p70; p71; p72]) in
  let bird = cycle ~style:JLine 
               (fold_append ~style:JLine 
                  [line1 ; line6; line2; line8; line4; line7])
  in
    [ Command.iter (-1) 1
        (fun x ->
           Command.iter (-1) 1
             (fun y ->
                let xf, yf = float_of_int x, float_of_int y in
                let offset = (xf *. xs1 +. yf *. xs2, yf *. ys) in
                let offset2 = ( (xf +. 1.) *. xs1 +. (yf -. 1.) *. xs2, 
                                (yf -. 1.) *. ys  ) in
                let tr p = Path.shift (bpp offset) p in
                let mypath po = 
                  let offset = add offset2 po in
                    Path.shift (bpp offset) Path.fullcircle
                in
                  seq ([ fill ~color:Color.red (mypath (-12.,27.));
			 draw ~color:Color.blue (mypath (-12.,27.))] @
			 [ fill ~color:mygreen (tr bird)] @
			 List.map (fun p -> draw ~pen:pen1 (tr p)) 
			 [line1;line2;line3;line4;line5] @
			 List.map (fun p -> draw ~pen:pen1 (tr p)) 
			 [line6; line7; line8] ) ) ) ]
