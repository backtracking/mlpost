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

type t = num

let f x = F x

let num_of_int i = F (float_of_int i)

let bp f = F f
let pt f = F (0.99626 *. f)
let cm f = F (28.34645 *. f)
let mm f = F (2.83464 *. f)
let inch f = F (72. *. f)

let pi = 3.14159
let deg2rad f = pi *. f /. 180.

let is_zero f = abs_float f < 0.00001

type scale = float -> t

module Scale = struct
  let bp x y = bp (x *. y)
  let pt x y = pt (x *. y)
  let cm x y = cm (x *. y)
  let mm x y = mm (x *. y)
  let inch x y = inch (x *. y)
end

module Infix = struct
  let (+/) x y = NAdd (x,y)
  let (-/) x y = NMinus (x,y)
  let ( */) x y = NMult (x,y)
  let (//) x y = NDiv (x,y)
end
