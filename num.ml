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

module F = Format

type t = 
  | BP of float
  | PT of float
  | CM of float
  | MM of float
  | IN of float

let bp f = BP f
let pt f = PT f
let cm f = CM f
let mm f = MM f
let inch f = IN f

let print_float fmt f =
  if f = infinity then F.fprintf fmt "infinity"
  else
    if f > 4095. then F.fprintf fmt "%4f" 4095.
    else F.fprintf fmt "%.4f" f

let print fmt = function
  | BP f -> F.fprintf fmt "%a" print_float f 
  | PT f -> F.fprintf fmt "%apt" print_float f 
  | CM f -> F.fprintf fmt "%acm" print_float f 
  | MM f -> F.fprintf fmt "%amm" print_float f 
  | IN f -> F.fprintf fmt "%ain" print_float f 
