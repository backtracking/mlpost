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
open Num
open Command
open Format
open Helpers
open Point
open Path
module T = Transform

let (++) x y = pt (cm x, cm y)
let shift x y = transform [Transform.shifted (x ++ y)]

let () = Random.init 1234

open Tree
open Box

let d1 = 
  let a = Box.circle (Picture.tex "$\\sqrt2$") in
  let b = 
    Box.shift (2. ++ 0.) (Box.rect ~fill:Color.purple (Picture.tex "$\\pi$"))
  in
  let pen = Pen.default ~tr:[Transform.scaled (bp 3.)] () in
  [ Box.draw a;
    Box.draw b;
    Command.draw
      ~color:Color.red
      (Path.shift (1. ++ 1.) (Box.bpath a));
    draw_label_arrow ~color:Color.orange ~pen 
      ~pos:`Upright (Picture.tex "foo") (Box.west a) (Box.south_east b);
    box_arrow ~color:Color.blue a b;
  ]

open Box

let d2 =
  let b = 
    hbox ~padding:(bp 10.) ~pos:`Top ~stroke:(Some Color.red) ~dx:(bp 2.)
      ~dy:(bp 2.)
      [vbox ~padding:(bp 4.) ~pos:`Right [tex "A"; tex "BC"; tex "D"];
       vbox ~padding:(bp 4.) ~pos:`Left [tex "E"; tex "FGH"]]
  in
  [draw ~debug:false b;
   box_arrow (nth 1 (nth 0 b)) (nth 0 (nth 1 b))]

let proval =
  let f = 7. in
  let pen = Pen.square ~tr:[T.yscaled (bp 0.5); T.rotated 40.] () in
  let check = jointpath [-1.2,1.2; 0., -2. ; 2., 2. ; 5., 5.] [jLine ; jCurve; jCurve] in
    [ fill ~color:(Color.gray 0.2) (Path.scale (Num.bp f) fullcircle) ;
      label ~pos:`Left (Picture.tex "Pr") (Point.p (f /. (-4.),0.)) ;
      label ~pos:`Right (Picture.tex "al") (Point.p (f /. 4.,0.)) ;
      Command.draw ~color:Color.green ~pen check;]

open Tree

let yannick style =
  let tt s = Box.tex ~style ~fill:Color.orange ("\\texttt{" ^ s ^ "}") in
  let node s = node ~ls:(bp 20.) ~cs:(bp 10.) ~edge_style:Square (tt s) in
  let leaf s = leaf (tt s) in

  let tree =
    node "ComposerPage"
      [ leaf "MemSet";
    node "ComposerMessages"
      [ node "ComposerMsg"
          [ leaf "StrCpy"; leaf "DeclarerPanneRobustesse" ]
      ]
      ]
  in
  [draw tree]


let rec random_tree ?arrow_style ?edge_style ?stroke ?pen n = 
  let tex = tex ~fill:Color.yellow in
  match n with
  | 1 -> leaf (tex "1")
  | 2 -> 
      node ?arrow_style ?edge_style 
        (Box.tex ~style:Box.Rect ~fill:(Color.rgb 0.5 0.3 0.2) "2") 
        [leaf (tex "1")]
  | n -> 
      let k = 1 + Random.int (n - 2) in
      node ?arrow_style ?edge_style ?stroke ?pen
        (tex (string_of_int n)) 
        [random_tree k; random_tree (n - 1 - k)]

let d2c, d2s, d2sq, d2hsq = 
(*   let ls = bp (-1.0) in *)
  let stroke = Color.blue and pen = Pen.circle () and arrow_style = Directed in
  [draw 
    (random_tree ~edge_style:Curve ~arrow_style ~stroke ~pen 17)],
  [draw 
    (random_tree ~edge_style:Straight ~arrow_style ~stroke ~pen 17)],
  [draw 
    (random_tree ~edge_style:Square ~arrow_style ~stroke ~pen 17)],
  [draw 
    (random_tree ~edge_style:HalfSquare ~arrow_style ~stroke ~pen 17)]

let d5 = 
  let rand_tree name i = set_name name (set_stroke Color.black (random_tree i)) in
  let t1 = rand_tree "1" 5 in
  let t2 = rand_tree "2" 6 in
  let bl = Box.hbox ~padding:(Num.cm 2.) [ box t1; box t2] in
  let b1 = nth 0 (get "1" bl) in
  let b2 = nth 0 (nth 1 (get "2" bl)) in
  [ draw bl; box_arrow b1 b2; ]


let tree1 () = pic (Picture.make (draw (random_tree (1 + Random.int 5))))

let rec random_tree2 = function
  | 1 -> leaf (tree1 ())
  | 2 -> node ~cs:(mm 0.2) (tree1 ()) [leaf (tree1 ())]
  | n -> 
      let k = 1 + Random.int (n - 2) in
      node ~cs:(mm 0.2) (tree1 ()) [random_tree2 k; random_tree2 (n - 1 - k)]

let d6 = [draw  (random_tree2 10)]
let cheno011 =
  let p = path ~cycle:jCurve [(0.,0.); (30.,40.); (40.,-20.); (10.,20.)] in
  let pen = Pen.circle ~tr:[T.scaled (bp 1.5)] () in
  [Command.draw p;
   seq (List.map
	   (fun (pos, l, i) -> 
	     Command.dotlabel ~pos (Picture.tex l) (point i p))
	   [`Bot, "0", 0.;  `Upleft, "1", 1. ;
	    `Lowleft, "2", 2. ;  `Top, "3", 3. ; `Left, "4", 4. ]);
   Command.draw ~pen (subpath 1.3 3.2 p)]

open Dash

let d3 =
  let p = pathp [cmp (0., 0.); cmp (5., 0.)] in
  let pat = pattern [on (bp 6.); off (bp 12.); on (bp 6.)] in
  [Command.draw p ~dashed:pat]


let pic = Picture.make (seq cheno011)

let d4 = 
  [draw_pic pic;
   iter 1 5 
     (fun i -> 
	draw_pic (Picture.transform [T.rotated (10. *. float i)] pic))
  ]

let d7 =
  let pic = 
    Picture.transform [T.scaled (bp 4.)] (Picture.tex "bound this!") in
  let pbox = pathp ~style:jLine ~cycle:jLine
    [Picture.ulcorner pic; Picture.urcorner pic; 
     Picture.lrcorner pic; Picture.llcorner pic] in
    [Command.draw_pic pic;
     Command.draw (Picture.bbox pic);
     Command.draw pbox;
     Command.dotlabel ~pos:`Left (Picture.tex "ulcorner") (Picture.ulcorner pic);
     Command.dotlabel ~pos:`Left (Picture.tex "llcorner") (Picture.llcorner pic);
     Command.dotlabel ~pos:`Right (Picture.tex "urcorner") (Picture.urcorner pic);
     Command.dotlabel ~pos:`Right (Picture.tex "lrcorner") (Picture.lrcorner pic);
    ]
    
let half pic = Picture.transform [Transform.scaled (bp 0.5)] pic

let rec right_split n pic = 
  if n <= 0 then pic
  else
    let smaller = right_split (n-1) (half pic) in
      Picture.beside pic (Picture.below smaller smaller)

let d11 =
  let p1 = Picture.transform [Transform.rotated 90.] (Picture.tex "recursion") in
    [Command.draw_pic (right_split 4 p1)]

let rec sierpinski p n =
  if n = 0 then p else
    let sp = sierpinski p (n-1) in
    let p = half sp in
    let p1 = Picture.beside p p in
      Picture.below p p1

let d12 = 
  let p1 = Picture.tex "A" in
    [Command.draw_pic (sierpinski p1 7)]


(** plots *)
open Plot
let sk = mk_skeleton 20 14 (Num.bp 20.) (Num.bp 20.)

let d13 =  [ (draw_grid sk)]

let squaref x = x *. x
let f2 i = sqrt (float_of_int i)
let f3 i = squaref (float_of_int i)

let d14 =
  let hdash _ = Dash.scaled 0.5 Dash.withdots in
  let vdash _ = Dash.scaled 2. Dash.evenly in
  let hvpen i = 
    if i mod 5 = 0 then
      Pen.default ~tr:([Transform.scaled (Num.bp 2.5)]) ()
    else Pen.default () in
  let pen = Pen.default ~tr:[Transform.scaled (Num.bp 4.)] () in
     [draw_grid ~hdash ~vdash ~hpen:hvpen ~vpen:hvpen sk;
      draw_func ~pen f2 sk;
      draw_func ~pen f3 sk
     ]

let f1 i =
  let aux = function
  | 0 -> 1
  | 1 | 2 -> 2
  | 3 | 4 -> 3
  | 5  -> 4
  | 6 | 7 -> 5
  | 8 |9 -> 6
  | 10 -> 7
  | 11 | 12 -> 8
  | 13 | 14 -> 9
  | 15 -> 10
  | 16 | 17 -> 11
  | 18 | 19 -> 12
  | 20 -> 13
  | _ -> 0
  in
    float_of_int (aux i)

let f2 i =
  let aux = function
  | 0 | 1 | 2 -> 0
  | 3 -> 1
  | 4 -> 2
  | 5 | 6 | 7 -> 3
  | 8 -> 4
  | 9 -> 5
  | 10 | 11 | 12 -> 6
  | 13 -> 7 
  | 14 -> 8
  | 15 | 16 | 17 -> 9
  | 18 -> 10
  | 19 -> 11
  | 20 -> 12
  | _ -> 0
  in
    float_of_int (aux i)

let f3 i =
  float_of_int ((i+3)/5)

let flab i = (Picture.transform
		[Transform.scaled (bp 1.7)]
		(Picture.tex (Printf.sprintf "$f_{\\omega_%d}$" i)),
	      `Top, 19)
	
let instants = 
  let pen = Pen.default ~tr:[Transform.scaled (bp 2.5)] () in
  let base = 
    Command.draw ~pen (Path.path ~style:jLine [(0.,-65.); (280.,-65.)]) in
  let tick i = 
    let xi = float_of_int i *. 14. in
    let yi = if f1 i = f1 (i-1) then -60. else -45. in
    let p = Path.path ~style:jLine [(xi,-65.); (xi, yi)] in
      Command.draw ~pen p
  in
    Command.seq 
      [base; Command.iter 0 20 tick; 
       Command.label (Picture.transform [Transform.scaled two]
			(Picture.tex "$\\omega_1$")) (p (-20., -55.))]

let florence =
  let sk = mk_skeleton 20 14 (bp 14.) (bp 20.) in
  let pen = Pen.default ~tr:[Transform.scaled (bp 4.)] () in
  let pen2 = Pen.default ~tr:[Transform.scaled (bp 3.)] () in
  let dash _ = Dash.scaled 0.5 Dash.withdots in
  let dash2 = Dash.scaled 0.66 Dash.withdots in
  let dash3 = Dash.scaled 0.9 Dash.evenly in
  let vcaption, hcaption = 
    let tr = [Transform.scaled (bp 1.5)] in
      Picture.transform tr (Picture.tex "\\textsf{Number of ones}"),
    Picture.transform tr (Picture.tex "\\textsf{Instants}") in
  let plot = draw_func ~drawing:Stepwise ~style:jLine in
  [ draw_grid ~hdash:dash ~vdash:dash ~color:(Color.gray 0.5) sk;
    draw_axes ~closed:true ~hcaption ~vcaption sk;
    plot ~pen ~label:(flab 1) f1 sk;
    plot ~pen:pen2 ~dashed:dash2 ~label:(flab 2) f2 sk;
    plot ~pen ~dashed:dash3 ~label:(flab 3) f3 sk;
    instants
  ]

let shapes1 =
  List.fold_left Picture.below
    (Picture.tex "Shapes 1 !")
    [Shapes.rectangle (bp 10.) (bp 20.);
     Shapes.rectangle ~stroke:Color.purple ~thickness:4. (bp 35.) (bp 15.);
     Shapes.rectangle 
       ~fill:Color.blue ~stroke:Color.orange ~thickness:4. (bp 15.) (bp 35.);
     Shapes.rounded_rect (bp 55.) (bp 25.) (bp 10.) (bp 10.);
     Shapes.rounded_rect ~stroke:Color.green (bp 55.) (bp 25.) (bp 20.) (bp 5.);
(*      Shapes.rounded_rect ~fill:Color.black ~stroke:Color.red  *)
(*        ~thickness:2. 70. 25. 12.5 12.5; *)
     Shapes.rounded_rect ~fill:Color.black ~stroke:Color.red 
       ~thickness:2. (bp 70.) (bp 25.) (bp 14.) (bp 14.);
    ]
  
let shapes2 =
  List.fold_left Picture.below
    (Picture.tex "Shapes 2 !")
    [
(*
      Shapes.arc_ellipse (f 10.) (f 10.) 0. 1.7;
      Shapes.arc_ellipse ~stroke:Color.red (f 30.) (f 10.) 0. 1.7;
      Shapes.arc_ellipse ~stroke:Color.red ~close:true (f 30.) (f 10.) 0. 1.7;
      Shapes.arc_ellipse 
	~fill:Color.black ~stroke:Color.red (f 30.) (f 10.) 0. 1.7;
*)
      Shapes.ellipse (bp 10.) (bp 10.);
      Shapes.ellipse ~stroke:Color.red (bp 30.) (bp 10.);
      Shapes.ellipse ~fill:Color.black ~stroke:Color.red (bp 30.) (bp 10.);
    ]

let figs = [
  d6; d5; yannick Box.Rect; yannick Box.Patatoid; d1; d2; proval;
              d2sq; d2hsq; d2s; d2c; d12; cheno011; d3; d4;
             d7; d11; florence;
            [Command.draw_pic shapes1]; [Command.draw_pic shapes2];
             d14; d13;
] 

let figs =
  let r = ref 0 in
  List.map (fun f -> incr r; !r, f) figs

(* CM fonts do not scale well *)

let theprelude = "\\documentclass[a4paper]{article}
\\usepackage[T1]{fontenc}
\\usepackage{times}
"


let () =
  Metapost.generate_mp ~prelude:theprelude "test/tests.mp" figs;
  Misc.write_to_formatted_file "test/tests.tex"
    (fun fmt ->
      fprintf fmt "\\documentclass[a4paper]{article}@.";
      fprintf fmt "\\usepackage[T1]{fontenc}@.";
      fprintf fmt "\\usepackage{times}@.";
      fprintf fmt "\\usepackage[]{graphicx}@.";
      fprintf fmt "@[<hov 2>\\begin{document}@.";
      List.iter
        (fun (i,_) ->
          fprintf fmt "@\n %i\\quad" i;
	  fprintf fmt "\\includegraphics[width=\\textwidth,height=\\textwidth,keepaspectratio]{tests.%d}" i;
          fprintf fmt "@\n \\vspace{3cm}@\n"
        ) figs;
      fprintf fmt "@]@\n\\end{document}@.")
