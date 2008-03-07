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

module F = Format

type direction = 
  | Vec of Point.t
  | Curl of float
  | NoDir 

type joint = 
    JLine
  | JCurve
  | JCurveNoInflex
  | JTension of float * float
  | JControls of Point.t * Point.t

type knot = direction * Point.t * direction

(* the intention is to add new knots in front,
 * i. e. the list has to be reversed for printing *)
type t =
  | Concat of knot * joint * t
  | Cycle of direction * joint * t
  | FullCircle
  | HalfCircle
  | QuarterCircle
  | UnitSquare
  | Transformed of t * Transform.t
  | Knot of knot
  | Append of t * joint * t
  | BoxBPath of Box.t
  | CutAfter of t * t
  | CutBefore of t * t
  | BuildCycle of t list

let start k = Knot k
let concat p j k = Concat (k,j,p)
let cycle d j p = Cycle (d,j,p)
let append p1 j p2 = Append (p1,j,p2)
let fullcircle = FullCircle
let halfcircle = HalfCircle
let quartercircle = QuarterCircle
let unitsquare = UnitSquare
let transform tr = function
  | Transformed (p,tr') -> Transformed (p,tr'@tr)
  | _ as x -> Transformed (x,tr)
let bpath b = BoxBPath b
let cut_after p1 p2 = CutAfter (p1, p2)
let cut_before p1 p2 = CutBefore (p1, p2)
let build_cycle l = BuildCycle l

let print_joint fmt = function
  | JLine -> F.fprintf fmt "--"
  | JCurve -> F.fprintf fmt ".."
  | JCurveNoInflex -> F.fprintf fmt "..."
  | JTension (a,b) -> 
      F.fprintf fmt "..tension %a and %a .." Num.print_float a Num.print_float b
  | JControls (a,b) -> 
      F.fprintf fmt "..controls %a and %a .." Point.print a Point.print b

let print_dir fmt = function
  | NoDir -> ()
  | Vec p -> F.fprintf fmt "{%a}" Point.print p
  | Curl f -> F.fprintf fmt "{curl %a}" Num.print_float f

let print_knot fmt (d1,p,d2) = 
  F.fprintf fmt "%a%a%a" print_dir d1 Point.print p print_dir d2

let rec print_list sep prf fmt = function
  | [] -> ()
  | [x] -> prf fmt x
  | (x::xs) -> prf fmt x; sep fmt (); print_list sep prf fmt xs

let rec print fmt = function
  | FullCircle -> F.fprintf fmt "fullcircle"
  | HalfCircle -> F.fprintf fmt "halfcircle"
  | QuarterCircle -> F.fprintf fmt "quartercircle"
  | UnitSquare -> F.fprintf fmt "unitsquare"
  | Transformed (p,tr) -> F.fprintf fmt "((%a) %a)"
      print p Transform.print tr
  | Append (p1,j,p2) -> 
      F.fprintf fmt "%a %a@ %a" print p1 print_joint j print p2
  | Cycle (d,j,p) ->
      F.fprintf fmt "%a %a %acycle" print p print_joint j print_dir d
  | Concat (k,j,p) ->
      F.fprintf fmt "%a %a %a" print p print_joint j print_knot k
  | Knot k -> print_knot fmt k
  | BoxBPath b ->
      F.fprintf fmt "bpath.%a" Name.print (Box.name b)
  | CutAfter (p1, p2) -> F.fprintf fmt "%a cutafter %a@ " print p2 print p1
  | CutBefore (p1, p2) -> F.fprintf fmt "%a cutbefore %a@ " print p2 print p1
  | BuildCycle l ->
      F.fprintf fmt "buildcycle(%a)" 
        (print_list (fun fmt () -> F.fprintf fmt ",") print) l

let point f p =
  Point.unsafe (fun fmt -> F.fprintf fmt "(point %.4f of (%a))" f print p)
