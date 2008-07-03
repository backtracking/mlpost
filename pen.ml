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

open Types
open Transform

type t = Types.pen

let transform tr p = 
  match tr, p with
    | [], _ -> p
    | _, PenTransformed (t,tr') -> PenTransformed (t,tr'@tr)
    | _, _ -> PenTransformed (p,tr)

let default ?(tr=id) () = PenTransformed (PenCircle, (scaled 0.5)::tr)
let circle ?(tr=id) () = transform tr PenCircle
let square ?(tr=id) () = transform tr PenSquare
let from_path p = PenFromPath p


