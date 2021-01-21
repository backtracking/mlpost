let writef f file =
  let cout = open_out file in
  let fmt = Format.formatter_of_out_channel cout in
  f fmt;
  Format.pp_print_flush fmt ();
  close_out cout

let print_dune_project fmt =
  Fmt.pf fmt "(lang dune 2.7)"

let print_common_template fmt =
  Fmt.pf fmt "open! Mlpost@\n";
  Fmt.pf fmt "open! Box@\n"


let print_fig_template name fmt =
  Fmt.pf fmt "open Mlpost@\n";
  Fmt.pf fmt "open Box@\n";
  Fmt.pf fmt "open! Common_for_figures@\n";
  Fmt.pf fmt "let fig = draw (tex (Mlpost.Picture.escape_all %S))@\n" name;
  Fmt.pf fmt "let () = Metapost.emit %S fig@\n" name

let modes = ["mps";"pgf";"png"]

let additional_options mode =
  match mode with
  | "png" -> ["-cairo"]
  | _ -> []

let print_dune figures latex required fmt =
  let dep fmt s = Fmt.pf fmt "%%{dep:%s}" (Filename.concat ".." s) in
  Fmt.pf fmt "(executables (names %a) (libraries mlpost mlpost.options))@\n@\n"
    Fmt.(list ~sep:sp string) figures;
  List.iter (fun fig ->
      List.iter (fun mode ->
      Fmt.pf fmt "(rule (targets %S) (alias %s) (action (run %S -%s %a %a %a)) (mode promote-until-clean))"
        (fig^"."^mode)
        mode
        ("./"^fig^".exe")
        mode
        Fmt.(option (any "-latex " ++ dep)) latex
        Fmt.(list ~sep:sp (any "-required " ++ dep)) required
        Fmt.(list ~sep:sp string) (additional_options mode)
        ) modes) figures;
  ()

let run figures latex required outdir =
  Unix.mkdir outdir 0o770;
  writef print_dune_project (Filename.concat outdir "dune-project");
  List.iter
    (fun fig -> writef
        (print_fig_template fig)
        (Filename.concat outdir (fig ^ ".ml")))
    figures;
  writef (print_dune figures latex required) (Filename.concat outdir "dune");
  writef print_common_template (Filename.concat outdir "common_for_figures.ml");
  0

let cmd =
  let open Cmdliner in
  let figures = Arg.(value & pos_all string [] &
                     info [] ~docv:"FIG" ~doc:"mlpost figures to initialize") in
  let latex = Arg.(value & opt (some string) None &
                     info ["latex"] ~docv:"TEX" ~doc:"latex file to include") in
  let required = Arg.(value & opt_all string [] &
                     info ["required"] ~docv:"TEX" ~doc:"additional required files") in
  let outdir = Arg.(value & opt string "mlpost_figures" &
                     info ["dir"] ~docv:"DIR" ~doc:"directory to initialize") in
  Term.(const run $ figures $ latex $ required $ outdir)

let () = exit (Cmdliner.Cmd.(eval' (v (info "mlpost-init") cmd)))
