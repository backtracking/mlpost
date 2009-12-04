open Format
open Arg

let pdf = ref true
let latex_file = ref None
let set_latex_file f =
  if not (Sys.file_exists f) then begin
    eprintf "mlpost: %s: no such file@." f;
    exit 1
  end;
  latex_file := Some f
let xpdf = ref false
let eps = ref false
let verbose = ref false
let cairo = ref false
let t1disasm = ref None
let depend = ref false
let dumpable = ref false
let dont_clean = ref false
let mp = ref false
let png = ref false

(* notuple please or change Tool.wrap_options *)
let spec =
  (["-pdf", Set pdf, " Generate .mps files (default)";
    "-mp", Set mp, " Generate .mp";
    "-png", Set png, " Generate .png";
    "-ps", Clear pdf, " Generate .1 files";
    "-latex", String set_latex_file, "<main.tex> Scan the LaTeX prelude";
    "-eps", Set eps, " Generate encapsulated postscript files";
    "-xpdf", Set xpdf, " wysiwyg mode using xpdf remote server";
    "-v", Set verbose, " be a bit more verbose";
    "-depend", Set depend, " output dependency lines in a format suitable for the make(1) utility";
    "-dumpable", Set dumpable, " output one name of dumpable file by line";
    "-dont-clean", Set dont_clean, " Don't remove intermediate files";
    "-cairo" , Set cairo, " Use the experimental cairo backend instead of metapost";
    "-t1disasm" , String (fun s -> t1disasm := Some s), " Set the program used to decrypt PostScript Type 1 font, only with cairo (default built-in one)"])

