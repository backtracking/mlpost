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

open Diag

let d2 =
  let pen = Pen.circle in
  let a = node 0. 4. "\\phantom{A}" in
  let b = node 0. 3. "" in
  let inv = node 0. 2. "inv" in
  let c = node 0. 1. "" in
  let d = node 0. 0. "" in
  let do_ = node (-2.) 2. "do" in
  let diag = create [a;b;c;d;inv;do_] in
  let arrow = arrow diag in
  arrow a b ~lab:"$i\\leftarrow0$" ~pos:Pright;
  arrow b inv ~lab:"$m\\leftarrow t[i]$" ~pos:Pright;
  arrow c d ~lab:"$i\\ge n$" ~pos:Pright;
  arrow c do_ ~outd:Left ~ind:Down ~lab:"$i<n$" ~pos:Plowleft;
  arrow inv c ~lab:"$i\\leftarrow i+1$" ~pos:Pright;
  arrow do_ inv ~lab:"$m\\ge t[i]$" ~pos:Ptop;
  arrow do_ b ~outd:Up ~ind:Left ~lab:"$m<t[i]$" ~pos:Pupleft;
  draw ~fill:Color.yellow ~stroke:Color.blue ~pen diag
    
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
