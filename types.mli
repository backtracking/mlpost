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

type num = float

type color = float * float * float

type name = string

type corner = N | S | W | E | NE | NW | SW | SE

type position =
  | Pcenter
  | Pleft
  | Pright
  | Ptop
  | Pbot
  | Pupleft
  | Pupright
  | Plowleft
  | Plowright

type on_off = On of num | Off of num

type box_circle_style =
  | Padding of num * num (* dx , dy *)
  | Ratio of float (* dx / dy *)

type point = 
  | PTPair of num * num
  | PTBoxCorner of name * corner
  | PTPointOf of float * path
  | PTAdd of point * point
  | PTSub of point * point
  | PTMult of float * point
  | PTRotated of float * point

and direction = 
  | Vec of point
  | Curl of float
  | NoDir 

and joint = 
  | JLine
  | JCurve
  | JCurveNoInflex
  | JTension of float * float
  | JControls of point * point

and knot = direction * point * direction

and path =
  | PAConcat of knot * joint * path
  | PACycle of direction * joint * path
  | PAFullCircle
  | PAHalfCircle
  | PAQuarterCircle
  | PAUnitSquare
  | PATransformed of path * transform list
  | PAKnot of knot
  | PAAppend of path * joint * path
  | PABoxBPath of box
  | PACutAfter of path * path
  | PACutBefore of path * path
  | PABuildCycle of path list
  | PASub of float * float * path
  | PAName of name

and transform =
  | TRRotated of float
  | TRScaled of num
  | TRShifted of point
  | TRSlanted of num
  | TRXscaled of num
  | TRYscaled of num
  | TRZscaled of point
  | TRReflect of point * point
  | TRRotateAround of point * float

and picture = 
  | PITex of string
  | PIMake of command
  | PITransform of transform list * picture
  | PIName of name

and box =
  | BCircle of name * point * picture * box_circle_style option
  | BRect of name * point * picture

and dash =
  | DEvenly
  | DWithdots
  | DScaled of float * dash
  | DShifted of point * dash
  | DPattern of on_off list

and pen = 
  | PenCircle
  | PenSquare
  | PenFromPath of path
  | PenTransformed of pen * transform list

and command =
  | CDraw of path * color option * pen option * dash option
  | CDrawArrow of path * color option * pen option * dash option
  | CDrawPic of picture
  | CFill of path * color option
  | CLabel of picture * position * point
  | CDotLabel of picture * position * point
  | CLoop of int * int * (int -> command list)
  | CDrawBox of color option * box
  | CSeq of command list
  | CDeclPath of name * path
  | CDefPic of name * command

