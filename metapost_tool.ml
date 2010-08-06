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
  File.LowLevel.read_from file (fun c ->
    Scan_prelude.scan (Lexing.from_channel c))

let rnd_state = Random.State.make_self_init ()

(** create a temporary directory *)
let rec create_dir prefix suffix =
  try
    let i = Random.State.int rnd_state 10000 in
    let dir = File.Dir.from_string (Printf.sprintf "%s%i%s" prefix i suffix) in
    let dirname = File.Dir.concat File.Dir.temp dir in
    File.Dir.mk dirname 0o700; dirname
  with Unix.Unix_error (Unix.EEXIST, _, _) -> create_dir prefix suffix

(** create a temporary directory and call function [f] within.
 * [rename] is a map from file names to file names; after executing [f], for
 * each pair [(key,value)] in [rename], [tempdir]/[key] is moved to
 * [workdir]/[value] *)
let tempdir ?(clean=true) prefix suffix f rename =
  let tmpdir = create_dir prefix suffix in
  let workdir = File.Dir.cwd () in
  File.Dir.ch tmpdir;
  let res = f workdir tmpdir in
  File.Dir.ch workdir;
  File.Map.iter (fun k v ->
    let from = File.place tmpdir k in
    let to_ = File.place workdir v in
    File.move from to_) rename;
  if clean then File.Dir.rm tmpdir;
  res

let append_dir dir suffix =
  let dir =
    if Filename.check_suffix dir Filename.dir_sep
    then Filename.chop_suffix dir Filename.dir_sep
    else dir in
  dir^suffix

