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

  let spec =
    [ "-pdf", Set pdf, "generates .mps files";
      "-latex", String set_latex_file, 
      "<main.tex>  scans the LaTeX prelude";
    ]

  let () = 
    Arg.parse spec add_file "usage: mlpost [options] files..."

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
    let out = Sys.command s in
    if out <> 0 then begin
      eprintf "mlpost: the following command failed:@\n%s@." s;
      exit out
    end

  let ocaml args =
    let cmd = "ocaml " ^ String.concat " " (Array.to_list args) in
    let out = Sys.command cmd in
    if out <> 0 then exit 1

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
    let pdf = if !pdf then "" else "~pdf:true" in
    let prelude = match !latex_file with
      | None -> ""
      | Some f -> 
	  let c = open_in f in
	  let s = scan (from_channel c) in
	  close_in c;
	  sprintf "~prelude:%S" s
    in
    Printf.fprintf cout 
      "let () = Mlpost.Metapost.dump %s %s \"%s\"\n" prelude pdf bn;
    close_out cout;
    ocaml [|"mlpost.cma"; mlf|];
    Sys.remove mlf

  let () = Queue.iter compile files
}
