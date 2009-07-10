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

open Point_lib
type t = Cairo.matrix =
  { xx : float; yx : float; xy : float; yy : float; x0 : float; y0 : float; }

include Cairo.Matrix

let linear xx xy yx yy = 
  { xx = xx; xy = xy; yx = yx; yy = yy; x0 = 0.; y0 = 0. }

let translation p = init_translate p.x p.y
let xy_translation x y = init_translate x y
let rotation = init_rotate
let scale f = init_scale f f
let xscaled f = init_scale f 1.
let yscaled f = init_scale 1. f

let slanted f = linear 1. f 0. 1.
let zscaled p = linear p.x (0. -. p.y) p.y p.x
let reflect p1 p2 = (*TODO *) assert false
let rotate_around p f = 
  translate (rotate (translation (mult (-1.) p)) f) p.x p.y

let invert = invert

let to_cairo x = x
let identity = init_identity

let transform = Cairo.transform
let set = Cairo.set_matrix

let remove_translation t = { t with x0 = 0.; y0 = 0.}

let print fmt m = 
  Format.fprintf fmt "[|[|%f;%f|];[|%f;%f|];[|%f;%f|]|]" 
    m.xx m.xy m.yx m.yy m.x0 m.y0
