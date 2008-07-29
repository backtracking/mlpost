(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Johannes Kanig, Stephane Lescuyer                       *)
(*  and Jean-Christophe Filliatre                                         *)
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

type color = 
  | RGB of float * float * float
  | CMYK of float * float * float * float
  | Gray of float

type name = string

type corner = N | S | W | E | NE | NW | SW | SE
type piccorner = UL | UR | LL | LR

type boxed = Boxed | Unboxed

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

type num = 
  | F of float
  | NXPart of point
  | NYPart of point
  | NAdd of num * num
  | NMinus of num * num
  | NMult of num * num
  | NDiv of num * num
  | NMax of num * num
  | NMin of num * num
  | NGMean of num * num

and point = 
  | PTPair of num * num
  | PTPicCorner of picture * piccorner
  | PTPointOf of float * path
  | PTAdd of point * point
  | PTSub of point * point
  | PTMult of num * point
  | PTRotated of float * point
  | PTTransformed of point * transform list

and on_off = On of num | Off of num

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
  | PACutAfter of path * path
  | PACutBefore of path * path
  | PABuildCycle of path list
  | PASub of float * float * path
  | PABBox of picture
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
  | PIClip of picture * path

and box = 
  { c : point ; bpath : path; pic : picture; height : num ; width : num }

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
  | CLoop of int * int * (int -> command)
  | CDrawBox of color option * boxed * box
  | CSeq of command list
(*
  These have moved into the compiled AST
  | CDeclPath of name * path
  | CDefPic of name * command
    *)
