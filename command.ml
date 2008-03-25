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

open Misc
open Types
module T = Transform

type position = Types.position =
  | Pcenter
  | Pleft
  | Pright
  | Ptop
  | Pbot
  | Pupleft
  | Pupright
  | Plowleft
  | Plowright

type t = command

type figure = t list

let label ?(pos=Pcenter) pic point = CLabel (pic,pos,point)
(* replace later *)
let dotlabel ?(pos=Pcenter) pic point = CDotLabel (pic,pos,point)

let draw ?color ?pen ?dashed t = 
  (* We don't use a default to avoid the output of 
     ... withcolor (0.00red+0.00green+0.00blue) withpen .... 
     for each command in the output file *)
    CDraw (t, color, pen, dashed)

let draw_arrow ?color ?pen ?dashed t = CDrawArrow (t, color, pen, dashed)

let fill ?color t = CFill (t, color)

let iter from until f = CLoop (from, until, f)

let draw_box ?fill ?(boxed=true) b = 
  CDrawBox (fill, (if boxed then Boxed else Unboxed), b)

let draw_pic p = CDrawPic p

let append c1 c2 = CSeq [c1; c2]
let (++) = append
let seq l = CSeq l

(* syntactic sugar *)

let iterl f l = seq (List.map f l)

let nop = CSeq []

