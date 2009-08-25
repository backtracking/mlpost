(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Johannes Kanig, Stephane Lescuyer                       *)
(*  Jean-Christophe Filliatre, Romain Bardou and Francois Bobot           *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

open Types
open Hashcons

type t = num


let zero = mkF 0.
let one = mkF 1.
let minus_one = mkF (-.1.)
let two = mkF 2.

let num_of_int i = mkF (float_of_int i)

let bp f = mkF f
let pt f = mkF (0.99626 *. f)
let cm f = mkF (28.34645 *. f)
let mm f = mkF (2.83464 *. f)
let inch f = mkF (72. *. f)

let pi = 3.1415926535897932384626433832795029
let pi_div_180 = pi /. 180.0
let deg2rad f = pi_div_180 *. f 

let is_zero f = abs_float f < 0.0001

type scale = float -> t

let addn x y = 
  match x.node, y.node with
  | F f1, F f2 -> mkF (f1 +. f2)
  | _, F f when is_zero f -> x
  | F f, _ when is_zero f -> y
  | _, _ -> mkNAdd x y

let subn x y = 
  match x.node, y.node with
  | F f1, F f2 -> mkF (f1 -. f2)
  | _, F f  when is_zero f -> x
  | _, _ -> mkNSub x y

let multn x y = 
  match x.node, y.node with
  | F f1, F f2 -> mkF (f1 *. f2)
  | (F f, _ | _ , F f) when is_zero f -> zero
  | _, _ -> mkNMult x y

let multf f x = multn (mkF f) x
  
let divn x y = 
  match x.node, y.node with
  | F f1, F f2 -> mkF (f1 /. f2)
  | F f, _ when is_zero f -> zero
  | _, _ -> mkNDiv x y

let divf x f = divn x (mkF f)

let maxn x y =
  match x.node, y.node with
    | F f1, F f2 -> mkF (max f1 f2)
    | _, _ -> mkNMax x y

let minn x y =
  match x.node, y.node with
    | F f1, F f2 -> mkF (min f1 f2)
    | _, _ -> mkNMin x y

let gmean x y = 
  match x.node, y.node with
  | F f1, F f2 -> mkF ( sqrt (f1 *. f1 +. f2 *. f2 ))
  | _, F f when is_zero f -> x
  | F f, _ when is_zero f -> y
  | _, _ -> mkNGMean x y

let fold_max f = List.fold_left (fun w p -> maxn w (f p))

let if_null n n1 n2 =
  match n.node with
    | F f  when is_zero f -> n1
    | F f  -> n2
    | _ -> mkNIfnullthenelse n n1 n2

(*
let bpn n = n
let ptn n = multn (F 0.99626) n
let cmn n = multn (F 28.34645) n
let mmn n = multn (F 2.83464) n
let inchn n = multn (F 72.) n
*)

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
  let ( *./)  = multf
  let (/./)  = divf
end

open Infix

let neg x = zero -/ x

let abs x = maxn x (neg x)
