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

{ 
  open Format
  open Lexing
  open Arg

  let files = 
    Queue.create ()

  let add_file f = 
    if not (Filename.check_suffix f ".ml") then begin
      eprintf "mlpost: don't know what to do with %s@." f;
      exit 1
    end;
    if not (Sys.file_exists f) then begin
      eprintf "mlpost: %s: no such file@." f;
      exit 1
    end;
    Queue.add f files

  let pdf = ref false
  let latex_file = ref None
  let set_latex_file f =
    if not (Sys.file_exists f) then begin
      eprintf "mlpost: %s: no such file@." f;
      exit 1
    end;
    latex_file := Some f
  let xpdf = ref false
  let use_ocamlbuild = ref false
  let ccopt = ref ""
  let execopt = ref ""
  let eps = ref false
  let verbose = ref false

  let add_ccopt x = ccopt := !ccopt ^ " " ^ x
  let add_execopt x = execopt := !execopt ^ " " ^ x

  let spec = Arg.align
    [ "-pdf", Set pdf, " Generate .mps files";
      "-latex", String set_latex_file, "<main.tex> Scan the LaTeX prelude";
      "-eps", Set eps, " Generate encapsulated postscript files";
      "-xpdf", Set xpdf, " wysiwyg mode using xpdf";
      "-v", Set verbose, " be a bit more verbose";
      "-ocamlbuild", Set use_ocamlbuild, " Use ocamlbuild to compile";
      "-ccopt", String add_ccopt, 
      "\"<options>\" Pass <options> to the Ocaml compiler";
      "-execopt", String add_execopt,
      "\"<options>\" Pass <options> to the compiled program";
    ]

  let () = 
    Arg.parse spec add_file "Usage: mlpost [options] files..."

  let buffer = Buffer.create 1024
}

(* scan the main LaTeX file to extract its prelude *)

rule scan = parse
  | "\\%" as s
      { Buffer.add_string buffer s; scan lexbuf }
  | "%" [^'\n']* '\n'
      { Buffer.add_char buffer '\n'; scan lexbuf }
  | _ as c
      { Buffer.add_char buffer c; scan lexbuf }
  | "\\begin{document}"
      { Buffer.contents buffer }
  | eof 
      { Buffer.contents buffer }

{
  let command s =
    let () = if !verbose then Format.eprintf "%s@." s in
    let out = Sys.command s in
    if out <> 0 then begin
      eprintf "mlpost: the following command failed:@\n%s@." s;
      exit out
    end

  let ocaml args =
    let cmd = "ocaml " ^ String.concat " " (Array.to_list args) in
    let () = if !verbose then Format.eprintf "%s@." cmd in
    let out = Sys.command cmd in
    if out <> 0 then exit 1

  let ocamlbuild args =
    command ("ocamlbuild " ^ String.concat " " args)

  (** Return an unused file name which in the same directory as the prefix. *)
  let temp_file_name prefix suffix =
    if not (Sys.file_exists (prefix ^ suffix)) then
      prefix ^ suffix
    else begin
      let i = ref 0 in
      while Sys.file_exists (prefix ^ string_of_int !i ^ suffix) do
        incr i
      done;
      prefix ^ string_of_int !i ^ suffix
    end

  let compile f =
    let bn = Filename.chop_extension f in
    let mlf, cout = Filename.open_temp_file "mlpost" ".ml" in
    Printf.fprintf cout "open Mlpost\n";
    Printf.fprintf cout "# 1 \"%s\"\n" f;
    begin 
      let cin = open_in f in
      try while true do output_char cout (input_char cin) done
      with End_of_file -> ()
    end;
    let pdf = if !pdf || !xpdf then "~pdf:true" else "" in
    let eps = if !eps then "~eps:true" else "" in
    let prelude = match !latex_file with
      | None -> ""
      | Some f -> 
	  let c = open_in f in
	  let s = scan (from_channel c) in
	  close_in c;
	  sprintf "~prelude:%S" s
    in
    Printf.fprintf cout 
      "\nlet () = Mlpost.Metapost.dump %s %s %s \"%s\"\n" prelude pdf eps bn;
    if !xpdf then 
      Printf.fprintf cout 
	"\nlet () = Mlpost.Metapost.dump_tex %s \"_mlpost\"\n" prelude;
    close_out cout;

    if !use_ocamlbuild then begin
      (* Ocamlbuild cannot compile a file which is in /tmp *)
      let mlf2 = temp_file_name bn ".ml" in
      command ("cp " ^ mlf ^ " " ^ mlf2);
      ocamlbuild
        ["-lib mlpost"; Filename.chop_extension mlf2 ^ ".byte"; !ccopt; "--";
         !execopt];
      Sys.remove mlf2
    end else
      ocaml [|"mlpost.cma"; !ccopt; mlf; !execopt|];

    Sys.remove mlf;
    if !xpdf then begin
      ignore (Sys.command "pdflatex _mlpost.tex");
      ignore (Sys.command "xpdf -remote mlpost -reload")
(***	
      match Unix.fork () with
	| 0 -> 
	    begin match Unix.fork () with
	      | 0 -> eprintf "ICI@."; Unix.execvp "xpdf" [|"xpdf";"-remote"; "mlpost"; "_mlpost.pdf"|]
	      | _ -> exit 0
	    end
	| id -> 
	    ignore (Unix.waitpid [] id); exit 0
***)
    end

  let () = Queue.iter compile files
}
