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

type t = Types.color = 
  | RGB of float * float * float
  | CMYK of float * float * float * float
  | Gray of float
         
let red = RGB (1.0,0.0,0.0)
let blue = RGB (0.0,0.0,1.0)
let green = RGB (0.0,1.0,0.0)
let orange = RGB (1.0,0.4,0.0)
let purple = RGB (0.6,0.0,0.6)
let magenta = RGB (1.0,0.0,1.0)
let cyan =  RGB (0.0,1.0,1.0)
let yellow = RGB (1.0,1.0,0.0)
let gray f = Gray f
let white = gray 1.0
let black = gray 0.0
let default = black
let rgb r g b = RGB (r,g,b)
let cmyk c m y k = CMYK (c,m,y,k)
