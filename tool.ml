(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Johannes Kanig, Stephane Lescuyer                       *)
(*  Jean-Christophe Filliatre, Romain Bardou and Francois Bobot           *)
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

open Format
open Arg

let use_ocamlbuild = ref false
let ccopt = ref " "
let execopt = ref " "
let verbose = Mlpost_desc_options.verbose
let native = ref false
let libdir = ref Version.libdir
let compile_name = ref None
let dont_execute = ref false
let dont_clean = ref false
let add_nothing = ref false

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

let notcairo = Version.include_string = ""

let version () =
  (* The first line of the output should be the version number, and only the
   * version number! *)
  Format.printf "%s@." Version.version;
  Format.printf "mlpost %s compiled at %s@." Version.version Version.date;
  Format.printf "searching for mlpost.cm(a|xa) in %s@." Version.libdir;
  if not notcairo then 
    Format.printf "additional directories are %s@." Version.include_string;
  exit 0

let add_ccopt x = ccopt := !ccopt ^ x ^ " "
let add_execopt x = execopt := !execopt ^ x ^ " "

let give_lib () =
  if notcairo then ["","unix"] 
  else ("","unix")::("","bigarray")::Version.cairolibs

let get_include_compile s = 
  let aux = function
    | "cmxa" -> List.map (fun (x,y) -> Filename.concat x (y^".cmxa")) 
        (give_lib ())
    | "cma" -> List.map (fun (x,y) -> Filename.concat x (y^".cma")) 
        (give_lib ())
    | "dir" -> List.map fst (give_lib ())
    | "file" -> List.map snd (give_lib ())
    | _ -> assert false in
  print_string (String.concat "\n" (aux s))

let nocairo () =
  print_string "Mlpost has not been compiled with cairo\n";
  exit 1

let options_for_compiled_prog = Queue.create ()
let aotofcp ?arg s = 
  Queue.add s options_for_compiled_prog;
  match arg with
    | None -> ()
    | Some s -> Queue.add s options_for_compiled_prog

let execopt cmd =
  let b = Buffer.create 30 in
  bprintf b "%s %a -- %s" 
    cmd
    (fun fmt -> Queue.iter (fprintf fmt "\"%s\" ")) options_for_compiled_prog
    !execopt;
  Buffer.contents b

 (* The option have the same behavior but
    add itself to option_for_compiled_prog in addition *)
let wrap_option (opt,desc,help) = 
  let desc = 
    match desc with
      | Unit f -> Unit (fun () -> f ();aotofcp opt)
      | Set s -> Unit (fun () -> s:=true; aotofcp opt)
      | Clear s -> Unit (fun () -> s:=false; aotofcp opt)
      | String f -> String (fun s -> f s;aotofcp ~arg:s opt)
      | Int f -> Int (fun s -> f s;aotofcp ~arg:(string_of_int s) opt)
      | Float f -> Float (fun s -> f s;aotofcp ~arg:(string_of_float s) opt)
      | Bool f -> Bool (fun s -> f s;aotofcp ~arg:(string_of_bool s) opt)
      | Set_int s -> Int (fun x -> s:=x; aotofcp ~arg:(string_of_int x) opt)
      | Set_float s -> Float (fun x -> s:=x; aotofcp ~arg:(string_of_float x) opt)
      | Set_string s -> String (fun x -> s:=x; aotofcp ~arg:x opt)
      | Symbol (l, f) -> Symbol (l,fun x -> f x; aotofcp ~arg:x opt)
      | Rest _ | Tuple _ -> assert false (*Not implemented... *)
 in
  (opt,desc,help)


let spec = Arg.align
  (["-ocamlbuild", Set use_ocamlbuild, " Use ocamlbuild to compile";
    "-native", Set native, " Compile to native code";
    "-ccopt", String add_ccopt, 
    "\"<options>\" Pass <options> to the Ocaml compiler";
    "-execopt", String add_execopt,
    "\"<options>\" Pass <options> to the compiled program";
    "-version", Unit version, " Print Mlpost version and exit";
    "-libdir", String ((:=) libdir), " Set path for mlpost.cma";
    "-get-include-compile", Symbol (["cmxa";"cma";"dir";"file"],get_include_compile), " Output the libraries which are needed by the library Mlpost";
    "-compile-name", String (fun s -> compile_name := Some s), "<compile-name> Keep the compiled version of the .ml file";
    "-dont-execute", Set dont_execute, " Don't execute the mlfile";
    "-dont-clean", Set dont_clean, " Don't remove intermediate files";
    "-add-nothing", Set add_nothing, " Add nothing to the file (deprecated)"
  ]@(if notcairo 
     then ["-cairo" , Unit nocairo, " Mlpost has not been compiled with the cairo backend";
           "-t1disasm" , Unit nocairo, " Mlpost has not been compiled with the cairo backend";
          ]
     else [])
   @(List.map wrap_option Mlpost_desc_options.spec))

let () = 
  Arg.parse spec add_file "Usage: mlpost [options] files..."

exception Command_failed of int

let command' ?inv ?outv s =
  let s, _ = Misc.call_cmd ?inv ?outv ~verbose:!verbose s in
  if s <> 0 then raise (Command_failed s)

let command ?inv ?outv s = 
  try command' ?inv ?outv s with Command_failed s -> exit s

let execute ?outv cmd =
  let cmd = execopt cmd in
  if !dont_execute 
  then (if !verbose then printf "You can execute the program with :@.%s" cmd)
  else command ?outv cmd

let normalize_filename s =  if Filename.is_relative s then "./"^s else s

let get_exec_name compile_name = 
  match compile_name with 
    | None -> Filename.temp_file "mlpost" ""
    | Some s -> normalize_filename s

let try_remove s =
  try Sys.remove s with _ -> ()

let ocaml args =
(*  match !compile_name with
    | None when not !dont_execute -> 
        execute ~outv:true ("ocaml" ^ String.concat " " args)
    | _ -> *)
  let s = get_exec_name !compile_name in
  let cmd = "ocamlc" ^ String.concat " " args ^ " -o " ^ s in
  command ~outv:true cmd;
  execute ~outv:true s;
  match !compile_name with 
    | None -> if !dont_clean then () else Sys.remove s 
    | Some s -> ()

let ocamlopt bn args =
  let exe = get_exec_name !compile_name in
  let cmd = Version.ocamlopt^" -o " ^ exe ^ " " ^ String.concat " " args in
  command ~outv:true cmd;
  execute ~outv:true exe;
  match !compile_name with
    | None -> if !dont_clean then () else Sys.remove exe
    | Some s -> ()

let ocamlbuild args exec_name =
  let args = if !verbose then "-classic-display" :: args else " -quiet"::args in
  command ~outv:true ("ocamlbuild " ^ String.concat " " args ^ exec_name);
  execute ~outv:true ("_build/"^exec_name);
  (match !compile_name with
    | None -> ()
    | Some s -> command ("cp _build/" ^ exec_name ^ " " ^ s));
  if !dont_clean then () else command "ocamlbuild -clean"

let compile f =
  let bn = Filename.chop_extension f in
  if !use_ocamlbuild then begin
    let ext = if !native then ".native" else ".byte" in
    let exec_name = bn ^ ext in
    try
      let args = ["-lib unix"] in
      let args =
        if Version.libdir = "" then args else
          args@[sprintf "-cflags -I,%s -lflags -I,%s"
            Version.libdir Version.libdir] in
      let args = 
        if notcairo then args else
          let includecairos = 
            let cairolibs = List.map (fun (x,y) -> Filename.concat x y) Version.cairolibs in
            String.concat "," cairolibs in
          let iI = String.concat  ","
            (List.map (fun (x,_) -> "-I,"^x) Version.cairolibs) in
          args@[sprintf "-lflags %s -lib bigarray -libs %s" iI includecairos] in
      let args =
        args@["-no-links";"-lib mlpost";"-lib mlpost_desc_options";
              "-lib mlpost_options";!ccopt]
      in
      ocamlbuild args exec_name;
    with Command_failed out -> 
      exit out
  end else begin begin
    if !native then
      let cairo_args = if notcairo then [] 
      else [Version.include_string; "bigarray.cmxa"; "cairo.cmxa"; "bitstring.cmxa"] in
      ocamlopt bn ([!ccopt; "-I"; !libdir;"unix.cmxa"] @ cairo_args @ 
                     ["mlpost.cmxa";"mlpost_desc_options.cmxa";"mlpost_options.cmxa"; f])
    else
      let cairo_args = if notcairo then [] 
      else [Version.include_string; "bigarray.cma"; "cairo.cma"; "bitstring.cma"] in
      ocaml ([!ccopt; "-I"; !libdir;"unix.cma"] @ cairo_args @
               ["mlpost.cma";"mlpost_desc_options.cma";"mlpost_options.cma"; f])
  end;
    if not !dont_clean then List.iter (fun suf -> try_remove (bn^suf)) [".cmi";".cmo";".cmx";".o"]
  end

let () = Queue.iter compile files
