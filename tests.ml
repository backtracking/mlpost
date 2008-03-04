open Mlpost
open Num
open Command
open Point
open Format

let (++) x y = Point.p (cm x, cm y)
let shift x y = Path.transform [Transform.shifted (x ++ y)]

let d1 = 
  let a = Box.circle up (Picture.tex "A") in
  let b = Box.circle right (Picture.tex "B") in
  [ draw_box a;
    draw_box b;
    draw 
      ~color:Color.red
      (Path.transform [Transform.shifted (1. ++ 1.)] (Path.bpath a));
  ]

let figs = [d1]

let figs =
  let r = ref 0 in
  List.map (fun f -> incr r; !r, f) figs

let () =
  Generate.generate_mp "test/tests.mp" figs;
  Generate.write_to_formatted_file "test/tests.tex"
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
