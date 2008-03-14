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

type t = string

let create prefix =
  let r = ref 0 in
  fun () -> incr r; prefix ^ string_of_int !r

let node = create "node"

let rec alpha i =
  if 0 <= i && i <= 22 then String.make 1 (Char.chr (Char.code 'a' + i))
  else alpha (i / 22) ^ alpha (i mod 22)

let path =
  let r = ref 0 in
  fun () -> incr r; "path" ^ alpha !r

let picture = 
  let r = ref 0 in
  fun () -> incr r; "pic" ^ alpha !r

