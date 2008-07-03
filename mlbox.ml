
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
open Picture
open Point

type t = mlbox

let rect c p = 
  let pic = center p c in
    let p = bbox pic in
      MLBBox (c, p, pic)

let middle p1 p2 = segment 0.5 p1 p2

let margin = p (2., 2.)

(* the same behaviour as METAPOST *)
let get_margins ur ll =
  let diag = mult (F 0.5) (sub ur ll) in
  let l = add (lengthpoint diag) margin in
    sub l diag

let circle c pic = 
  let pic = center pic c in
  let ul = ulcorner pic in
  let lr = lrcorner pic in
  let ll = llcorner pic in
  let ur = urcorner pic in
  let marginpoint = get_margins ur ll in
  let dxp = yscaled (F 0.) marginpoint in
  let dyp = xscaled (F 0.) marginpoint in
  let halfxdiff = add (mult (F 0.5) (sub lr ll)) dxp in
  let halfydiff = add (mult (F 0.5) (sub ul ll)) dyp in
  let e = add c halfxdiff in
  let w = sub c halfxdiff in
  let s = sub c halfydiff in
  let n = add c halfydiff in
  let path = Path.pathk ~cycle:Path.JCurveNoInflex ~style:Path.JCurveNoInflex
              (List.map2 (fun p d -> (NoDir,p,Vec d)) 
              [e; n; w; s;] [up; left; down; right; ])  in
    MLBBox (c, path, pic)

let center (MLBBox (c,_,_)) = c

let north_west (MLBBox (_,_,pic)) = ulcorner pic
let north_east  (MLBBox (_,_,pic)) = urcorner pic
let south_west (MLBBox (_,_,pic)) = llcorner pic
let south_east  (MLBBox (_,_,pic)) = lrcorner pic

let north b = segment 0.5 (north_west b) (north_east b)
  
let south b = segment 0.5 (south_west b) (south_east b) 
let west  b = segment 0.5 (north_west b) (north_west b) 
let east  b = segment 0.5 (south_east b) (north_east b) 

let bpath (MLBBox (_,p,_)) = p
