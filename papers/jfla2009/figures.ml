
(* figures pour l'article JFLA *)

open Mlpost
open Command
open Picture
open Path
module H = Helpers
open Num
open Num.Infix
open Point

(* Stéphane *)
let graph_sqrt =
  let u = Num.cm in
  let pen = Pen.circle ~tr:[Transform.scaled one] () in 
  let rec pg = function
    | 0 -> start (knot ~r:(vec up) ~scale:u (0.,0.))
    | n -> let f = (float_of_int n /. 2.) in 
	concat ~style:jCurve (pg (n-1)) (knot ~scale:u (f, sqrt f)) 
  in
  [draw (pathn ~style:jLine [(zero,u 2.); (zero,zero); (u 4.,zero)]);
   draw ~pen (pg 8);
   label ~pos:`Lowright (tex "$ \\sqrt x$") (pt (u 3., u (sqrt 3.)));
   label ~pos:`Bot (tex "$x$") (pt (u 2., zero));
   label ~pos:`Lowleft (tex "$y$") (pt (zero, u 1.))]

let architecture =
  [nop]

(* Romain *)
let automate =
  [nop]

(* Johannes *)
let diagramme =
  [nop]

(* JC *)
let bresenham =
  [nop]

let () = Metapost.emit "automate" automate
let () = Metapost.emit "diagramme" diagramme
let () = Metapost.emit "graph_sqrt" graph_sqrt
let () = Metapost.emit "architecture" architecture
let () = Metapost.emit "bresenham" bresenham




