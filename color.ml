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
  
type spec =
  | RGB of float * float * float
  | Gray of float
      (* O-ary colors ^^ *)
  | Black
  | White
      (* primary colors *)
  | Red
  | Green
  | Blue
      (* secondary colors *)
  | Yellow
  | Cyan
  | Magenta
      (* er.. help me out there ! *)
  | Orange
  | Purple
         
let default = (0.0, 0.0, 0.0)

let make = function
  | RGB(r,g,b) -> r,g,b
  | Gray f -> f,f,f
  | Black -> 0.0,0.0,0.0
  | White -> 1.0,1.0,1.0
  | Red -> 1.0,0.0,0.0
  | Green -> 0.0,1.0,0.0
  | Blue -> 0.0,0.0,1.0
  | Yellow -> 1.0,1.0,0.0
  | Cyan -> 0.0,1.0,1.0
  | Magenta -> 1.0,0.0,1.0
  | Orange -> 1.0,0.4,0.0
  | Purple -> 0.6,0.0,0.6

let red = make Red
let orange = make Orange
let blue = make Blue
let purple = make Purple

let print fmt (r,g,b) =
  F.fprintf fmt "(%.3f red + %.3f green + %.3f blue)" r g b
    
