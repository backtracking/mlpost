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

let rnd_state = Random.State.make_self_init ()

(** delete a directory that may contain files *)
let rmdir dir =
  Array.iter (fun x -> Sys.remove (Filename.concat dir x)) (Sys.readdir dir);
  Unix.rmdir dir

(** copy a file to another *)
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

(** rename a file *)
let file_move src dest =
  try Unix.rename src dest
  with Unix.Unix_error (Unix.EXDEV,_,_) -> file_copy src dest

(** create a temporary directory *)
let rec create_dir prefix suffix =
  try
    let i = Random.State.int rnd_state 10000 in
    let dirname = Filename.concat Filename.temp_dir_name
                    (Printf.sprintf "%s%i%s" prefix i suffix) in
    Unix.mkdir dirname  0o700; dirname
  with Unix.Unix_error (Unix.EEXIST, _, _) -> create_dir prefix suffix

(** create a temporary directory and call function [f] within.
 * [rename] is a map from file names to file names; after executing [f], for
 * each pair [(key,value)] in [rename], [tempdir]/[key] is moved to
 * [workdir]/[value] *)
let tempdir ?(clean=true) prefix suffix f rename =
  let tmpdir = create_dir prefix suffix in
  let workdir = Sys.getcwd () in
  Sys.chdir tmpdir;
  let res = f workdir tmpdir in
  Sys.chdir workdir;
  Misc.StringMap.iter (fun k v ->
    let from = Filename.concat tmpdir k in
    let to_ = Filename.concat workdir v in
    file_move from to_) rename;
  if clean then rmdir tmpdir;
  res
