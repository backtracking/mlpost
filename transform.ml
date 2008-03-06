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

type t' = 
    Rotated of float
  | Scaled of Num.t
  | Shifted of Point.t
  | Slanted of Num.t
  | Xscaled of Num.t
  | Yscaled of Num.t
  | Zscaled of Point.t
  | Reflect of Point.t * Point.t
  | RotateAround of Point.t * float

type t = t' list

let scaled ?(scale=Num.bp) a = Scaled (scale a)
let rotated a = Rotated a
let shifted a = Shifted a
let slanted a = Slanted a
let xscaled a = Xscaled a
let yscaled a = Yscaled a
let zscaled a = Zscaled a
let reflect p1 p2 = Reflect (p1,p2)
let rotate_around p f = RotateAround (p,f)

let print_item fmt = function
  | Scaled a -> F.fprintf fmt "scaled %a@ " Num.print a
  | Shifted a -> F.fprintf fmt "shifted %a@ " Point.print a
  | Rotated a -> F.fprintf fmt "rotated %a@ " Num.print_float a
  | Slanted a -> F.fprintf fmt "slanted %a@ " Num.print a
  | Xscaled a -> F.fprintf fmt "xscaled %a@ " Num.print a
  | Yscaled a -> F.fprintf fmt "yscaled %a@ " Num.print a
  | Zscaled a -> F.fprintf fmt "zscaled %a@ " Point.print a
  | Reflect (p1,p2) -> 
      F.fprintf fmt "reflectedabout (%a,%a)@ " 
        Point.print p1 Point.print p2
  | RotateAround (p,f) ->
      F.fprintf fmt "rotatedaround(%a,%a)@ "
        Point.print p Num.print_float f

let rec print_list sep prf fmt = function
  | [] -> ()
  | [x] -> prf fmt x
  | (x::xs) -> prf fmt x; sep fmt (); print_list sep prf fmt xs

let print fmt l =
  print_list (fun fmt () -> F.fprintf fmt " ") print_item fmt l

let id = []
