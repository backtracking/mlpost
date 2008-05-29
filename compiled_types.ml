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

type point =
  | PTPair of num * num
  | PTBoxCorner of box * corner
  | PTPicCorner of picture * piccorner
  | PTPointOf of float * path
  | PTAdd of point * point
  | PTSub of point * point
  | PTMult of float * point
  | PTRotated of float * point
  | PTTransformed of point * transform list

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
  (* PASub only takes a name *)
  | PASub of float * float * name
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
  (* compiled pictures do not have PIMake *)
  | PITex of string
  | PITransform of transform list * picture
  | PIName of name

and box =
  | BName of name
and declbox =
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
  | CLoop of int * int * (int -> command)
  | CDrawBox of color option * boxed * box
  | CSeq of command list
  | CDeclPath of name * path
  | CDefPic of name * command
  | CDeclBox of declbox


and color = Types.color
and position = Types.position
and num = Types.num
and name = Types.name
and corner = Types.corner
and piccorner = Types.piccorner
and boxed = Types.boxed
and box_circle_style  =Types.box_circle_style
and on_off  =Types.on_off
