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

(* look for some piece of TeX *)

open Types
open Hashcons

let num_memoize = Hashtbl.create 50
let point_memoize = Hashtbl.create 50
let metapath_memoize = Hashtbl.create 50
let path_memoize = Hashtbl.create 50
let picture_memoize = Hashtbl.create 50
let command_memoize = Hashtbl.create 50

let clear () = 
  Hashtbl.clear num_memoize;
  Hashtbl.clear point_memoize;
  Hashtbl.clear metapath_memoize;
  Hashtbl.clear path_memoize;
  Hashtbl.clear picture_memoize;
  Hashtbl.clear command_memoize

let memoize f memoize =
  fun acc arg -> 
    try
      Hashtbl.find memoize arg.tag;
      acc
    with
        Not_found -> 
          Hashtbl.add memoize arg.tag ();
          f acc arg.node

let option_compile f acc = function
  | None -> acc
  | Some obj -> f acc obj

let rec num' acc = function
  | F f -> acc
  | NXPart p | NYPart p -> point acc p
  | NAdd(n1,n2) | NSub(n1,n2) | NMult (n1,n2)
  | NDiv (n1,n2) | NMax (n1,n2) | NMin (n1,n2)
  | NGMean (n1,n2) -> num (num acc n1) n2
  | NLength p -> path acc p
  | NIfnullthenelse (n,n1,n2) -> num (num (num acc n) n1) n2

and num acc = memoize num' num_memoize acc
and point' acc = function
  | PTPair (f1,f2) -> num (num acc f1) f2
  | PTPointOf (f,p) | PTDirectionOf (f,p)-> path (num acc f) p
  | PTAdd (p1,p2) |PTSub (p1,p2) -> point (point acc p1) p2
  | PTMult (f,p) -> point (num acc f) p
  | PTRotated (_,p) -> point acc p
  | PTPicCorner (pic,_) -> commandpic acc pic
  | PTTransformed (p,tr) -> point (transform acc tr) p
and point acc = memoize point' point_memoize acc
and knot acc k = 
  match k.Hashcons.node with
    |{ knot_in = d1 ; knot_p = p ; knot_out = d2 } ->
       direction (direction (point acc p) d1) d2
and joint acc j =
  match j.Hashcons.node with
      | JControls (p1,p2) -> point (point acc p1) p2
      | JLine|JCurve|JCurveNoInflex|JTension _ -> acc
and direction acc d = 
  match d.Hashcons.node with
    | Vec p -> point acc p
    | Curl _ | NoDir -> acc
and metapath' acc = function
  | MPAConcat (pa,j,p) -> metapath (knot (joint acc j) pa) p
  | MPAAppend (p1,j,p2) -> metapath (metapath (joint acc j) p1) p2
  | MPAKnot k -> knot acc k
  | MPAofPA p -> path acc p
and metapath acc = memoize metapath' metapath_memoize acc
and path' acc = function
  | PAofMPA p -> metapath acc p
  | MPACycle (d,j,p) -> direction (metapath (joint acc j) p) d
  | PATransformed (p,tr) -> path (transform acc tr) p
  | PACutAfter (p1,p2) |PACutBefore (p1,p2) -> path (path acc p1) p2
  | PASub (f1,f2,p) -> num (num (path acc p) f1) f2
  | PABBox p -> commandpic acc p
  | PABuildCycle p -> List.fold_left path acc p
  | PAUnitSquare | PAQuarterCircle | PAHalfCircle | PAFullCircle -> acc
and path acc = memoize path' path_memoize acc
and picture acc arg =
  try
    Hashtbl.find picture_memoize arg.tag;
    acc
  with
      Not_found -> 
        Hashtbl.add picture_memoize arg.tag ();
        match arg.node with
          | PITransformed (p,tr) -> commandpic (transform acc tr) p
          | PITex tex -> (arg,tex)::acc
          | PIClip (pic,pth) -> commandpic (path acc pth) pic
and transform acc t =
  match t.Hashcons.node with
    | TRRotated _ -> acc
    | TRScaled f | TRSlanted f | TRXscaled f | TRYscaled f -> num acc f
    | TRShifted p | TRZscaled p | TRRotateAround (p,_)-> point acc p
    | TRReflect (p1,p2) -> point (point acc p1) p2
    | TRMatrix p -> 
        num (num (num (num (num (num acc p.x0) p.y0) p.xx) p.xy) p.yx) p.yy
and dash acc d =
  match d.Hashcons.node with
    | DEvenly | DWithdots -> acc
    | DScaled (n,d) -> dash (num acc n) d
    | DShifted (p,d) -> point (dash acc d) p
    | DPattern l -> List.fold_left dash_pattern acc l
and dash_pattern acc o =
  match o.Hashcons.node with
    | On f | Off f -> num acc f
and command' acc = function
  | CDraw (p, b) -> 
      let {color = _; pen = pe; dash = dsh} = b.Hashcons.node in
      path ((option_compile pen) ((option_compile dash) acc dsh) pe) p
  | CFill (p,_) -> path acc p
  | CDotLabel (pic,_,pt) | CLabel (pic,_,pt) -> commandpic (point acc pt) pic
  | CExternalImage _ -> acc
and pen acc p = 
  match p.Hashcons.node with
    | PenCircle | PenSquare -> acc
    | PenFromPath p -> path acc p
    | PenTransformed (p,tr) -> pen (transform acc tr) p

and command acc = memoize command' command_memoize acc

and commandpic acc p =
  match p.Hashcons.node with
  | Picture p -> picture acc p
  | Command c -> command acc c
  | Seq l -> List.fold_left commandpic acc l

let compile_tex l =
  let tags,texs = List.split l in
  let texs = Gentex.create !Compute.prelude texs in
  List.iter2 (fun tag tex -> Hashtbl.add 
                Compute.picture_memoize tag.tag (Picture_lib.tex tex))
    tags texs

let ct_aux f = fun arg -> compile_tex (f [] arg)
let ct_auxl f = fun argl -> compile_tex (List.fold_left f [] argl)

let commandl arg = ct_auxl command arg; List.map Compute.command arg
let commandpicl arg = ct_auxl commandpic arg; List.map Compute.commandpic arg
let numl arg = ct_auxl num arg; List.map Compute.num arg
let pointl arg = ct_auxl point arg; List.map Compute.point arg
let pathl arg = ct_auxl path arg; List.map Compute.path arg
let metapathl arg = ct_auxl metapath arg; List.map Compute.metapath arg
let picturel arg = ct_auxl picture arg; List.map Compute.picture arg

let commandl_error ferror arg = ct_auxl command arg; List.map (ferror Compute.command) arg
let commandpicl_error ferror arg = ct_auxl commandpic arg; List.map (ferror Compute.commandpic) arg
let numl_error ferror arg = ct_auxl num arg; List.map (ferror Compute.num) arg
let pointl_error ferror  arg = ct_auxl point arg; List.map (ferror Compute.point) arg
let pathl_error ferror  arg = ct_auxl path arg; List.map (ferror Compute.path) arg
let metapathl_error ferror  arg = ct_auxl metapath arg; List.map (ferror Compute.metapath) arg
let picturel_error ferror arg = ct_auxl picture arg; List.map (ferror Compute.picture) arg



let compute_nums () =
  let l = ref [] in
  (fun n -> l:= num !l n),(fun () -> compile_tex !l;l:=[])


let commandpic arg = ct_aux commandpic arg; Compute.commandpic arg
let command arg = ct_aux command arg; Compute.command arg
let num arg = ct_aux num arg; Compute.num arg
let point arg = ct_aux point arg; Compute.point arg
let path arg = ct_aux path arg; Compute.path arg
let metapath arg = ct_aux metapath arg; Compute.metapath arg
let picture arg = ct_aux picture arg; Compute.picture arg
let transform arg = ct_auxl transform arg; 
  List.fold_left (fun acc t -> Matrix.multiply acc (Compute.transform t)) 
    Matrix.identity arg
