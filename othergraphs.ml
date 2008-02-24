open Path
open Point
open Helpers
open Mlpost
module C = Convenience
module P = Pen
module T = Transform

let a = 0., 0.
let b = 1., 0.
let c = 0., 1.
let l = [a ; b ; c]
let d1 = 1, [C.draw ~style:JLine ~scale:C.CM l]
let d2 = 2, [C.draw ~style:JLine ~scale:C.CM ~cycle:JLine 
                l]

let d4 = 4, 
         let pen = P.transform [T.scaled (Num.bp 4.)] Pen.circle in
           [C.draw ~pen:pen ~scale:C.CM [a] ]

let d5 = 5,
         let pen = P.transform [T.scaled (Num.bp 4.)] Pen.circle in
           [C.draw ~style:JLine ~scale:C.CM ~cycle:JLine l] @
           (List.map  (fun point -> C.draw ~pen:pen ~scale:C.CM [point]) l)

let d12 = 12,
          let pen = P.transform [T.scaled (Num.bp 2.)] Pen.circle in
          let cl = 
            List.map (fun f -> Color.make (Color.RGB (f,f,f) ) ) 
              [0.8;0.6;0.4] 
          in
            List.map2
              (fun (a,b) col ->
                 C.draw ~style:JLine ~scale:C.CM ~pen:pen ~color:col [a;b])
              [a,b;b,c;c,a] cl

let triangle = 
  C.path ~scale:C.CM ~style:JLine ~cycle:JLine [(0.,0.);(1.,0.);(0.,1.)]

let d20 =
  20, [fill ~color:(Color.make (Color.Gray 0.8)) triangle]

let d21 =
  21, [fill ~color:(Color.make (Color.Gray 0.8)) triangle; draw triangle]

let d22 =
  let pen = Pen.transform [T.scaled (Num.bp 2.)] Pen.circle in
  22, [fill ~color:(Color.make (Color.Gray 0.8)) triangle; 
       draw ~pen triangle]

let d23 =
  let pen = Pen.transform [T.scaled (Num.bp 2.)] Pen.circle in
  23, [draw ~pen triangle;
       fill ~color:(Color.make (Color.Gray 0.8)) triangle]

let deuxpi = 2.*.3.14159

let d130 =
  let sq = C.path ~style:JLine ~scale:C.CM ~cycle:JLine 
    [(0.,0.);(2.,0.);(2.,2.);(0.,2.)] in
    (** on peut pas utiliser la resolution de MetaPost donc on
	construit la transform à la main.. :-/ *)
  let ratio = sqrt (3.28 /. 4.) in
  let angle = atan (0.2 /. 1.8) *. 360. /. deuxpi in
  let v = p (Num.cm 0.2, Num.cm 0.) in
  let t = [T. rotated angle; T.scaled (Num.bp ratio); T.shifted v] in
  let rec apply acc = function 0 -> acc | n -> apply (transform t acc) (n-1) in
  let cmd i = 
    let p = apply sq (2*i) in
      [fill ~color:(Color.make (Color.Gray 0.8)) p;
       fill ~color:(Color.make Color.White) (transform t p)]
  in
    130, [iter 0 50 cmd]

let d140 =
  let cmd i =
    let s = 1. -. ((float_of_int i) *. 0.01) in
    let color = Color.make (Color.Gray s) in
      [fill ~color (transform [T.scaled (Num.cm (2.*.s))] fullcircle)]
  in
  140, [iter 0 100 cmd; 
	draw ~pen:(P.transform [T.scaled (Num.bp 2.)] P.circle)
	  (transform [T.scaled (Num.cm 2.)] fullcircle)]

let d149 =
  let step = deuxpi /. 720. in
  let couleur x =
    Color.make (
      let dblx = 2.*.x in
	if x > 0.5 then Color.RGB (dblx-.1.,0.,2.-.dblx)
	else Color.RGB (1.-.dblx,0.,dblx)) in
  let pt angle = (2.*.sin(2.*.angle), 2.*.cos(3.*.angle)) in
  let pen = P.transform [T.scaled (Num.bp 2.)] Pen.circle in
  let cmd i =
    let angle = step *. (float_of_int i) in
      [C.draw ~scale:C.CM ~color:(couleur (angle /. deuxpi)) ~pen [pt angle]]
  in
    (149,[Mlpost.iter 0 720 cmd])

let d195 = 
  let n = 8 and u x = 5.*. (float_of_int x) in
  let un = u n and u1 = u 1 and udemi = (u 1) /. 5. in
  let color = Color.make (Color.Gray 0.8) in
  let t i j = T.shifted (p (Num.mm (u i), Num.mm (u j))) in
  let row i =
    let col j =
      if (i+j) mod 2 = 1 then
	let strip k =
	  let kf = (float_of_int k) *. udemi in let umk = u1 -. kf in
	  let udp = kf +. udemi and udm = umk -. udemi in
	  let l = 
	    if k mod 2 = 1 then [(kf,0.); (u1,umk); (u1,udm); (udp,0.)]
	    else [(0.,kf); (umk,u1); (udm,u1); (0.,udp)]
	  in [fill ~color (transform [t i j]
			     (C.path ~style:JLine ~scale:C.MM ~cycle:JLine l))]
	in
	  [Mlpost.iter 0 5 strip]
      else [] 
    in 
      [Mlpost.iter 0 n col]
  in
  let grid i =
    let ui = u i in
      [C.draw ~style:JLine ~scale:C.MM [(0.,ui); (un, ui)];
       C.draw ~style:JLine ~scale:C.MM [(ui,0.); (ui, un)]]
  in
    (195, [Mlpost.iter 0 n row; Mlpost.iter 0 (n+1) grid])


let figs = 
  [ d1; d2; d4; d5; d12; d20; d21; d22; d23; d130; d140; d149; d195]

let mpostfile = "test/othergraphs.mp"
let texfile = "test/othergraphs.tex"

let _ =
    Generate.generate_mp mpostfile figs;
    Generate.generate_tex texfile "othergraphs/mpost" "othergraphs" figs


