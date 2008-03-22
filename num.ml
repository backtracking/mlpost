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

type t = float

let bp f = f
let pt f = 0.99626 *. f
let cm f = 28.34645 *. f
let mm f = 2.83464 *. f
let inch f = 72. *. f

let pi = 3.14159
let deg2rad f = pi *. f /. 180.

type scale = float -> t

module Scale = struct
  let bp x y = bp (x *. y)
  let pt x y = pt (x *. y)
  let cm x y = cm (x *. y)
  let mm x y = mm (x *. y)
  let inch x y = inch (x *. y)
end
