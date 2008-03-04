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

open Format

type name = string

let new_name =
  let r = ref 0 in
  fun () -> incr r; "node" ^ string_of_int !r

type t = 
  | Circle of name * Point.t * Picture.t
  | Rect of name * Point.t * Picture.t

let circle c p = Circle (new_name (), c, p)
let rect c p = Rect (new_name (), c, p)

let center = function
  | Circle (_, c, _) 
  | Rect (_, c, _) -> c

let declare fmt = function
  | Circle (n, c, p) -> 
      fprintf fmt "circleit.%s(%a);" n Picture.print p;
      fprintf fmt "%s.c = %a;@\n" n Point.print c
  | Rect (n, c, p) -> 
      fprintf fmt "boxit.%s(%a);" n Picture.print p;
      fprintf fmt "%s.c = %a;@\n" n Point.print c

let name = function
  | Circle (n, _, _) | Rect (n, _, _) -> n

