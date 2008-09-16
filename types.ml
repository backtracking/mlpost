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
  | NSub of num * num
  | NMult of num * num
  | NDiv of num * num
  | NMax of num * num
  | NMin of num * num
  | NGMean of num * num
  | NLength of path

and point = 
  | PTPair of num * num
  | PTPicCorner of picture * piccorner
  | PTPointOf of float * path
  | PTDirectionOf of float * path
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

and knot = 
    { knot_in : direction ; knot_p : point ; knot_out : direction }

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
  | PIClip of picture * path

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
  | CSeq of command list


open Hashtbl

let combine n acc = acc * 65599 + n

let combine2 n acc1 acc2 = combine n (combine acc1 acc2)

let combine3 n acc1 acc2 acc3 = combine n (combine acc1 (combine acc2 acc3))

let combine4 n acc1 acc2 acc3 acc4 = combine n (combine3 acc1 acc2 acc3 acc4)


let rec num = function 
  | F f -> combine 1 (hash f)
  | NXPart p -> combine 2 (point p)
  | NYPart p -> combine 3 (point p)
  | NAdd(n,m) -> combine2 4 (num n) (num m)
  | NSub(n,m) -> combine2 5 (num n) (num m)
  | NMult(n,m) -> combine2 6 (num n) (num m)
  | NDiv(n,m) -> combine2 7 (num n) (num m)
  | NMax(n,m) -> combine2 8 (num n) (num m)
  | NMin(n,m) -> combine2 9 (num n) (num m)
  | NGMean(n,m) -> combine2 10 (num n) (num m)
  | NLength p -> combine 11 (path p)

and point = function
  | PTPair(n,m) -> combine2 12 (num n) (num m)
  | PTPicCorner(p,pc) -> combine2 13 (picture p) (hash pc)
  | PTPointOf(f,p) -> combine2 14 (hash f) (path p)
  | PTDirectionOf(f,p) -> combine2 15 (hash f) (path p)
  | PTAdd(p,q) -> combine2 16 (point p) (point q)
  | PTSub(p,q) -> combine2 17 (point p) (point q)
  | PTMult(n,q) -> combine2 18 (num n) (point q)
  | PTRotated(f,p) -> combine2 19 (hash f) (point p)
  | PTTransformed(p,l) ->
      List.fold_left (fun acc t -> combine2 21 acc (transform t))
	(combine 20 (point p)) l
(*
and on_off = On of num | Off of num
*)

and direction = function
  | Vec p -> combine 61 (point p)
  | Curl f -> combine 62 (hash f)
  | NoDir -> 63

and joint = hash
(*
  | JLine
  | JCurve
  | JCurveNoInflex
  | JTension of float * float
  | JControls of point * point
*)

and knot k = 
  combine3 64 (direction k.knot_in) (point k.knot_p) (direction k.knot_out)


and path = function
  | PAConcat(k,j,p) -> 
      combine3 22 (knot k) (joint j) (path p)
  | PACycle(d,j,p) ->
      combine3 23 (direction d) (joint j) (path p)
  | PAFullCircle -> 24
  | PAHalfCircle -> 25
  | PAQuarterCircle -> 26
  | PAUnitSquare -> 27
  | PATransformed(p,l) ->
      List.fold_left (fun acc t -> combine2 28 acc (transform t))
	(combine 29 (path p)) l
  | PAKnot k -> combine 30 (knot k)
  | PAAppend(p1,j,p2) -> combine3 31 (path p1) (joint j) (path p2)
  | PACutAfter(p,q) -> combine2 32 (path p) (path q)
  | PACutBefore(p,q) -> combine2 33 (path p) (path q)
  | PABuildCycle l ->
      List.fold_left (fun acc t -> combine2 35 acc (path t)) 34 l
  | PASub(f1,f2,p) ->
      combine3 36 (hash f1) (hash f2) (path p)
  | PABBox p -> combine 37 (picture p)

and transform = function
  | TRRotated f -> combine 52 (hash f)
  | TRScaled n -> combine 53 (num n)
  | TRShifted p -> combine 57 (point p)
  | TRSlanted n -> combine 54 (num n)
  | TRXscaled n -> combine 55 (num n)
  | TRYscaled n -> combine 56 (num n)
  | TRZscaled p -> combine 58 (point p)
  | TRReflect(p,q) -> combine2 59 (point p) (point q)
  | TRRotateAround(p,q) -> combine2 60 (point p) (hash q)

and picture = function
  | PITex s -> combine 38 (hash s)
  | PIMake c -> combine 39 (command c)
  | PITransform(l,p) ->
      List.fold_left (fun acc t -> combine2 40 acc (transform t))
	(combine 41 (picture p)) l
  | PIClip(p,q) -> combine2 42 (picture p) (path q)

and dash = hash
(*
  | DEvenly
  | DWithdots
  | DScaled of float * dash
  | DShifted of point * dash
  | DPattern of on_off list
*)
and pen =  hash
(*
  | PenCircle
  | PenSquare
  | PenFromPath of path
  | PenTransformed of pen * transform list
*)
and command = function
  | CDraw(pa,c,p,d) ->
      combine4 43 (path pa) (hash c) (hash p) (hash d)
  | CDrawArrow(pa,c,p,d) ->
      combine4 44 (path pa) (hash c) (hash p) (hash d)
  | CDrawPic p -> combine 45 (picture p)
  | CFill(p,c) -> combine2 46 (path p) (hash c)
  | CLabel(pic,pos,poi) -> combine3 47 (picture pic) (hash pos) (point poi)
  | CDotLabel(pic,pos,poi) -> 
      combine3 48 (picture pic) (hash pos) (point poi) 
  | CLoop(n,m,_) ->  combine2 49 n m
  | CSeq l ->
      List.fold_left (fun acc t -> combine2 50 acc (command t)) 51 l



(* smart constructors *)

(* num *)

let mkF f = F f 

let mkNAdd n1 n2 = NAdd(n1,n2)

let mkNSub n1 n2 = NSub(n1,n2)

let mkNMult n1 n2 = NMult(n1,n2)

let mkNDiv n1 n2 = NDiv(n1,n2)

let mkNMax n1 n2 = NMax(n1,n2)

let mkNMin n1 n2 = NMin(n1,n2)

let mkNGMean n1 n2 = NGMean(n1,n2)

let mkNXPart p = NXPart p

let mkNYPart p = NYPart p

let mkNLength p = NLength p

(* point *)

let mkPTPair f1 f2 = PTPair(f1,f2)

let mkPTAdd p1 p2 = PTAdd(p1,p2)

let mkPTSub p1 p2 = PTSub(p1,p2)

let mkPTMult x y = PTMult(x,y)

let mkPTRotated x y = PTRotated(x,y)

let mkPTTransformed x y = PTTransformed(x,y)

let mkPTPointOf f p = PTPointOf(f,p)

let mkPTDirectionOf f p = PTDirectionOf(f,p)

let mkPTPicCorner x y = PTPicCorner(x,y)

(* transform *)

let mkTRScaled n = TRScaled n

let mkTRXscaled n = TRXscaled n

let mkTRYscaled n = TRYscaled n

let mkTRZscaled pt = TRZscaled pt

let mkTRRotated f = TRRotated f

let mkTRShifted pt = TRShifted pt

let mkTRSlanted n = TRSlanted n

let mkTRReflect pt1 pt2 = TRReflect(pt1,pt2)

let mkTRRotateAround pt f = TRRotateAround(pt,f)

(* knot *)

let mkKnot d1 p d2 = { knot_in = d1; knot_p = p; knot_out = d2 }

(* path *)

let mkPAKnot k = PAKnot k

let mkPAConcat p1 j p2 = PAConcat(p1,j,p2)

let mkPACycle p1 j d = PACycle (p1,j,d)

let mkPAAppend x y z = PAAppend (x,y,z)

let mkPAFullCircle = PAFullCircle

let mkPAHalfCircle = PAHalfCircle

let mkPAQuarterCircle = PAQuarterCircle

let mkPAUnitSquare = PAUnitSquare

let mkPATransformed x y = PATransformed (x,y)

let mkPACutAfter x y = PACutAfter (x,y)

let mkPACutBefore x y = PACutBefore (x,y)

let mkPABuildCycle l = PABuildCycle l

let mkPASub x y z = PASub (x,y,z)

let mkPABBox pic = PABBox pic

(* joint *)

let mkJCurve  = JCurve

let mkJLine  = JLine

let mkJCurveNoInflex  = JCurveNoInflex

let mkJTension x y = JTension(x,y)

let mkJControls x y = JControls(x,y)


(* direction *)

let mkNoDir  = NoDir

let mkVec p = Vec p

let mkCurl f = Curl f

(* picture *)

let mkPITex s = PITex s

let mkPIMake com = PIMake com

let mkPITransform x y = PITransform (x,y)

let mkPIClip p pic = PIClip(p,pic)

(* command *)

let mkCDraw x y z t = CDraw(x,y,z,t)

let mkCDrawArrow x y z t = CDrawArrow(x,y,z,t)

let mkCDrawPic pic = CDrawPic pic

let mkCFill x y = CFill(x,y)

let mkCLabel x y z = CLabel(x,y,z)

let mkCDotLabel x y z = CDotLabel(x,y,z)

let mkCLoop x y z = CLoop(x,y,z)

let mkCSeq l = CSeq l

(* dash *)

let mkDEvenly = DEvenly

let mkDWithdots = DWithdots

let mkDScaled x y = DScaled(x,y)

let mkDShifted x y = DShifted(x,y)

let mkDPattern l = DPattern l

(* pen *)

let mkPenCircle = PenCircle

let mkPenSquare = PenSquare

let mkPenFromPath p = PenFromPath p

let mkPenTransformed x y = PenTransformed(x,y)

(* on_off *)

let mkOn f = On f

let mkOff f = Off f

