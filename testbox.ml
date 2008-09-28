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
  let b = tabularl ~hpadding:(bp 10.) ~vpadding:(bp 5.) ~pos:`Right
    [[tex "a"; tex "BB"; tex "C"]; 
     [tex "ddd"; tex "\\tiny e"; tex "FF"]] in
  [draw ~debug:true b]

let figs = [
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
