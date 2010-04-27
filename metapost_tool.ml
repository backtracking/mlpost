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

let read_prelude_from_tex_file file =
  let c = open_in file in
  let s = Scan_prelude.scan (Lexing.from_channel c) in
  close_in c;
  s

(* f is called with f currdir tempdir *)
let tempdir ?(clean=true) prefix suffix f =
  let rec create_dir () =
    try
      let dirname = Filename.concat Filename.temp_dir_name
        (Printf.sprintf "%s%i%s" prefix (Random.int 10000) suffix) in
      Unix.mkdir dirname  0o700; dirname
    with | Unix.Unix_error (Unix.EEXIST, _, _) -> create_dir () in
  let dirname = create_dir () in
  let workdir_bak = Sys.getcwd () in
  Sys.chdir dirname;
  let res = f workdir_bak dirname in
  Sys.chdir workdir_bak;
  if clean then begin
    Array.iter (fun x -> Sys.remove (Filename.concat dirname x))
      (Sys.readdir dirname);
    Unix.rmdir dirname;
  end;
  res


let file_copy src dest =
  let cin = open_in src
  and cout = open_out dest
  and buff = String.make 1024 ' '
  and n = ref 0
  in
  while n := input cin buff 0 1024; !n <> 0 do
    output cout buff 0 !n
  done;
  close_in cin; close_out cout


let file_move src dest =
  try
    Unix.rename src dest
  with Unix.Unix_error (Unix.EXDEV,_,_) ->
    file_copy src dest
