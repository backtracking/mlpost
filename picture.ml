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

type t = picture

let tex s = PITex s
let make l = PIMake l
let bbox pic = PABBox pic
let ulcorner pic = PTPicCorner (pic, UL)
let llcorner pic = PTPicCorner (pic, LL)
let urcorner pic = PTPicCorner (pic, UR)
let lrcorner pic = PTPicCorner (pic, LR)
(* let currentpicture = PIName "currentpicture" *)

let transform tr = function
  | PITransform (tr', p) -> PITransform (tr'@tr, p)
  | _ as x -> PITransform (tr, x)
