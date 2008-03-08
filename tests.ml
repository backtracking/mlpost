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
      node "2" [leaf "1"]
  | n when Random.bool () -> 
      node (string_of_int n) [random_tree (n-1)]
  | n -> 
      let k = 1 + Random.int (n - 2) in
      node (string_of_int n) [random_tree k; random_tree (n - 1 - k)]

let d2 = 
  draw ~node_style:Circle ~arrow_style:Undirected
    ~fill:Color.yellow ~stroke:Color.blue ~pen:Pen.circle (random_tree 17)

 
let figs = [d2; d1]

let figs =
  let r = ref 0 in
  List.map (fun f -> incr r; !r, f) figs

let () =
  Command.generate_mp "test/tests.mp" figs;
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
