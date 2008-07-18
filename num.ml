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

let zero = f 0.
let one = f 1.
let two = f 2.

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

let addn x y = 
  match x, y with
  | F f1, F f2 -> F (f1 +. f2)
  | _, _ -> NAdd (x,y)

let subn x y = 
  match x, y with
  | F f1, F f2 -> F (f1 -. f2)
  | _, _ -> NMinus (x,y)

let multn x y = 
  match x, y with
  | F f1, F f2 -> F (f1 *. f2)
  | _, _ -> NMult (x,y)
  
let divn x y = 
  match x, y with
  | F f1, F f2 -> F (f1 /. f2)
  | _, _ -> NDiv (x,y)

let maxn x y =
  match x, y with
    | F f1, F f2 -> F (max f1 f2)
    | _, _ -> NMax (x,y)

let minn x y =
  match x, y with
    | F f1, F f2 -> F (min f1 f2)
    | _, _ -> NMin (x,y)

let gmean x y = 
  match x, y with
  | F f1, F f2 -> F ( sqrt (f1 *. f1 +. f2 *. f2 ))
  | _, _ -> NGMean (x,y)

let bpn n = n
let ptn n = multn (F 0.99626) n
let cmn n = multn (F 28.34645) n
let mmn n = multn (F 2.83464) n
let inchn n = multn (F 72.) n

module Scale = struct
  let bp x y = bp (x *. y)
  let pt x y = pt (x *. y)
  let cm x y = cm (x *. y)
  let mm x y = mm (x *. y)
  let inch x y = inch (x *. y)
end

module Infix = struct
  let (+/)  = addn
  let (-/)  = subn
  let ( */)  = multn
  let (//)  = divn
end
