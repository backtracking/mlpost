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
open Num
open Command
open Point
open Format
open Path
open Helpers
module SP = SimplePath
module T = Transform

let (++) x y = Point.p (cm x, cm y)
let shift x y = Path.transform [Transform.shifted (x ++ y)]

let d1 = 
  let a = Box.circle (0. ++ 0.) (Picture.tex "$\\sqrt2$") in
  let b = Box.rect (2. ++ 0.) (Picture.tex "$\\pi$") in
  let pen = Pen.transform [Transform.scaled 3.] Pen.default in
  [ draw_box a;
    draw_box ~fill:Color.purple b;
    draw
      ~color:Color.red
      (Path.transform [Transform.shifted (1. ++ 1.)] (Path.bpath a));
    draw_label_arrow ~color:Color.orange ~pen 
      ~pos:Pupright (Picture.tex "foo") (Box.west a) (Box.south_east b);
    box_simple_arrow ~color:Color.blue a b;
  ]

open Tree

let () = Random.self_init ()

let rec random_tree = function
  | 1 -> 
      leaf "1"
  | 2 -> 
      node ~style:Rect ~fill:Color.red "2" [leaf "1"]
  | n when Random.bool () -> 
      node (string_of_int n) [random_tree (n-1)]
  | n -> 
      let k = 1 + Random.int (n - 2) in
      node (string_of_int n) [random_tree k; random_tree (n - 1 - k)]

let d2c, d2s, d2sq, d2hsq = 
  let tree = random_tree 17 in
    (draw ~ls:(-1.0) 
       ~node_style:Circle ~arrow_style:Directed ~edge_style:Curve
       ~fill:Color.yellow ~stroke:Color.blue ~pen:Pen.circle tree,
     draw ~ls:(-1.0) 
       ~node_style:Circle ~arrow_style:Directed ~edge_style:Straight
       ~fill:Color.yellow ~stroke:Color.blue ~pen:Pen.circle tree,
     draw ~ls:(-1.0)
       ~node_style:Circle ~arrow_style:Directed ~edge_style:Square
       ~fill:Color.yellow ~stroke:Color.blue ~pen:Pen.circle tree,
     draw ~ls:(-1.0)
       ~node_style:Circle ~arrow_style:Directed ~edge_style:HalfSquare
       ~fill:Color.yellow ~stroke:Color.blue ~pen:Pen.circle tree)

let proval =
  let f = 7. in
  let pen = Pen.transform [T.yscaled 0.5; T.rotated 40.] Pen.square in
  let check = SP.jointpath [-1.2,1.2; 0., -2. ; 2., 2. ; 5., 5.] [JLine ; JCurve; JCurve] in
    [ fill ~color:Color.black 
        (Path.transform [Transform.scaled f] Path.fullcircle) ;
      Command.label ~pos:Pleft (Picture.tex "Pr") (Point.p (f /. (-4.),0.)) ;
      Command.label ~pos:Pright (Picture.tex "al") (Point.p (f /. 4.,0.)) ;
      Command.draw ~color:Color.green ~pen check;]

let cheno011 =
  let p = SP.path ~cycle:JCurve [(0.,0.); (30.,40.); (40.,-20.); (10.,20.)] in
  let pen = Pen.transform [T.scaled 1.5] Pen.circle in
  [Command.draw p;
   seq (List.map
	   (fun (pos, l, i) -> 
	     Command.dotlabel ~pos (Picture.tex l) (Path.point i p))
	   [Pbot, "0", 0.;  Pupleft, "1", 1. ;
	    Plowleft, "2", 2. ;  Ptop, "3", 3. ; Pleft, "4", 4. ]);
   Command.draw ~pen (Path.sub 1.3 3.2 p)]

open SimplePoint
open SimplePath
open Dash
open Transform

let d3 =
  let p = pathp [cmp (0., 0.); cmp (5., 0.)] in
  let pat = pattern [On (bp 6.); Off (bp 12.); On (bp 6.)] in
  [Command.draw p ~dashed:pat]


let pic = Picture.make cheno011

let d4 = 
  [Command.draw_pic pic;
   Command.iter 1 6 
     (fun i -> 
       [Command.draw_pic (Picture.transform [T.rotated (10. *. float i)] pic)])
  ]

let d5 = 
  let b1 = Box.circle (cmp (0.,0.)) pic in
  let b2 = Box.rect (cmp (10.,0.)) (Picture.transform [T.rotated 40.] pic) in
  [ draw_box b1;
    draw_box b2;
    box_arrow b1 b2 ]

let figs = [d5; d4; cheno011; proval; d3; d2sq; d2hsq; d2s; d2c; d1]

let figs =
  let r = ref 0 in
  List.map (fun f -> incr r; !r, f) figs

let () =
  Metapost.generate_mp "test/tests.mp" figs;
  Misc.write_to_formatted_file "test/tests.tex"
    (fun fmt ->
      fprintf fmt "\\documentclass[a4paper]{article}@.";
      fprintf fmt "\\usepackage[]{graphicx}@.";
      fprintf fmt "@[<hov 2>\\begin{document}@.";
      List.iter
        (fun (i,_) ->
          fprintf fmt "@\n %i\\quad" i;
	  fprintf fmt "\\includegraphics[width=\\textwidth,height=\\textwidth,keepaspectratio]{tests.%d}" i;
          fprintf fmt "@\n \\vspace{3cm}@\n"
        ) figs;
      fprintf fmt "@]@\n\\end{document}@.")
