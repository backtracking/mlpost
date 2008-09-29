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

open Box

let f0 =
  let b = 
    box ~style:Patatoid 
      (box ~style:Circle
	 (box ~style:Ellipse (box (tex "aBc")))) 
  in
  [draw ~debug:false b]

let f1 =
  let b = 
    hbox ~padding:(bp 20.)
      [vbox ~padding:(bp 4.) ~pos:`Right 
	 [tex "A"; tex ~name:"bc" "BC"; tex "D"];
       vbox ~padding:(bp 4.) ~pos:`Left  
	 [tex ~name:"e" "E"; tex "FGH"]]
  in
  [draw ~debug:false b;
   box_arrow (get "bc" b) (get "e" b)]

let f2 =
  let tex = tex ~style:Circle in
  let b = vbox [tex "a"; hbox [tex ~name:"b" "b"; tex "c"]] in
  let f = hbox ~padding:(bp 20.) [b;b;b] in
  let arrow = box_arrow ~outd:(vec (dir (-60.))) in
  let node i = get "b" (nth i f) in
  [draw ~debug:false f;
   arrow (node 0) (node 1); arrow (node 1) (node 2)]

let f3 =
  let b = tabularl ~hpadding:(bp 10.) ~vpadding:(bp 20.) ~pos:`Left
    [[tex "a"; tex "BB"; tex ~name:"dst" "C"]; 
     [tex ~name:"src" "ddd"; tex "\\tiny e"; tex "tagada"]] in
  [draw ~debug:true b;]

let f4 =
  let tex = tex ~stroke:None in
  let b = vblock ~pos:`Center [tex "a"; tex "b"; tex "c"] in
  [draw b]

let l = [[false; true; true; true; false; false; true];
	 [true; false; true; false; true; false; false];
	 [false; true; false; false; false; false; true]]
let f5 =
  let sz = Num.cm 1. in
  let empty = empty ~width:sz ~height:sz () in
  let black = set_fill (Color.gray 0.3) (set_stroke Color.black empty) in
  let white = set_stroke Color.black empty in
  let bit2box b = if b then black else white in
  let lb = List.map (List.map bit2box) l in
  [draw (tabularl lb)]

let sudoku =
  let sq33 cell = [[cell;cell;cell];[cell;cell;cell];[cell;cell;cell]] in
  let sz= Num.cm 0.5 in
  let cell = set_stroke Color.black (empty ~width:sz ~height:sz ()) in
  let pen = Pen.scale (Num.bp 1.) (Pen.circle ()) in
  let square = 
    set_stroke Color.black (set_pen pen (tabularl (sq33 cell))) in
    [draw (tabularl (sq33 square))]

let figs = [
  sudoku;
  f5;
  f4;
  f3;
  f0; f1; f2;
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
  Metapost.generate_mp ~prelude:theprelude "test/testbox.mp" figs;
  Misc.write_to_formatted_file "test/testbox.tex"
    (fun fmt ->
      fprintf fmt "\\documentclass[a4paper]{article}@.";
      fprintf fmt "\\usepackage[T1]{fontenc}@.";
      fprintf fmt "\\usepackage{times}@.";
      fprintf fmt "\\usepackage[]{graphicx}@.";
      fprintf fmt "@[<hov 2>\\begin{document}@.";
      List.iter
        (fun (i,_) ->
          fprintf fmt "@\n %i\\quad" i;
	  fprintf fmt "\\includegraphics[width=\\textwidth,height=\\textwidth,keepaspectratio]{testbox.%d}" i;
          fprintf fmt "@\n \\vspace{3cm}@\n"
        ) figs;
      fprintf fmt "@]@\n\\end{document}@.")
