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

open Format

let generate_tex tf tmpl1 tmpl2 l =
  let minipage fmt i tmpl =
    fprintf fmt "@[<hov 2>\\begin{minipage}[tb]{0.5\\textwidth}@\n";
    fprintf fmt "@[<hov 2>\\begin{center}@\n";
    fprintf fmt 
      "\\includegraphics[width=\\textwidth,height=\\textwidth,keepaspectratio]{%s.%i}" 
      tmpl i;
    fprintf fmt "@]@\n\\end{center}@\n";
    fprintf fmt "@]@\n\\end{minipage}@\n"
  in
    Misc.write_to_formatted_file tf
      (fun fmt ->
          fprintf fmt "\\documentclass[a4paper]{article}@.";
          fprintf fmt "\\usepackage[]{graphicx}@.";
          fprintf fmt "@[<hov 2>\\begin{document}@.";
          List.iter
            (fun (i,_) ->
               fprintf fmt "@\n %i" i;
               minipage fmt i tmpl1;
               minipage fmt i tmpl2;
               fprintf fmt "@\n \\vspace{3cm}@\n"
            ) l ;
          fprintf fmt "@]@\n\\end{document}@.")
