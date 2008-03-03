module F = Format

let write_to_file filename f =
  let chan = open_out filename in
    f chan;
    close_out chan

let write_to_formatted_file filename f =
  write_to_file filename
    (fun chan ->
      let fmt = F.formatter_of_out_channel chan in
        f fmt)

let generate_tex tf tmpl1 tmpl2 l =
  let minipage fmt i tmpl =
    F.fprintf fmt "@[<hov 2>\\begin{minipage}[tb]{0.5\\textwidth}@\n";
    F.fprintf fmt "@[<hov 2>\\begin{center}@\n";
    F.fprintf fmt 
      "\\includegraphics[width=\\textwidth,height=\\textwidth,keepaspectratio]{%s.%i}" 
      tmpl i;
    F.fprintf fmt "@]@\n\\end{center}@\n";
    F.fprintf fmt "@]@\n\\end{minipage}@\n"
  in
    write_to_formatted_file tf
      (fun fmt ->
          F.fprintf fmt "\\documentclass[a4paper]{article}@.";
          F.fprintf fmt "\\usepackage[]{graphicx}@.";
          F.fprintf fmt "@[<hov 2>\\begin{document}@.";
          List.iter
            (fun (i,_) ->
               F.fprintf fmt "@\n %i" i;
               minipage fmt i tmpl1;
               minipage fmt i tmpl2;
               F.fprintf fmt "@\n \\vspace{3cm}@\n"
            ) l ;
          F.fprintf fmt "@]@\n\\end{document}@.")

let generate_mp fn l =
  write_to_formatted_file fn
    (fun fmt -> List.iter (fun (i,f) -> Command.print i fmt f) l)

