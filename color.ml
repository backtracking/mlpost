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

type t = float * float * float
         
let red = 1.0,0.0,0.0
let blue = 0.0,0.0,1.0
let green = 0.0,1.0,0.0
let orange = 1.0,0.4,0.0
let purple = 0.6,0.0,0.6
let magenta = 1.0,0.0,1.0
let cyan =  0.0,1.0,1.0
let yellow = 1.0,1.0,0.0
let gray f = f,f,f
let white = gray 1.0
let black = gray 0.0
let default = black
let rgb r g b = (r,g,b)

let print fmt (r,g,b) =
  F.fprintf fmt "(%.3f red + %.3f green + %.3f blue)" r g b
    
