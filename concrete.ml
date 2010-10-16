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

IFDEF CONCRETE THEN
let supported = true

type cnum = float

module CPoint = Point_lib

module CPath =
  struct
    module S = Spline_lib
    type t = S.path
    type abscissa = float

    let length = S.metapost_length
    let is_closed = S.is_closed
    let is_a_point x = S.is_a_point x

    let c_metapost_of_abscissa p1 p2 (t1,t2) =
      S.metapost_of_abscissa p1 t1,
      S.metapost_of_abscissa p2 t2

    let intersection p1 p2 =
      List.map (c_metapost_of_abscissa p1 p2) (S.intersection p1 p2)

    let one_intersection p1 p2 =
      c_metapost_of_abscissa p1 p2 (S.one_intersection p1 p2)

    let reverse = S.reverse

    let iter = S.iter
    let fold_left = S.fold_left

    let cut_before = S.cut_before
    let cut_after = S.cut_after

    let split p t =  S.split p (S.abscissa_of_metapost p t)

    let subpath p t1 t2 =
      S.subpath p (S.abscissa_of_metapost p t1) (S.abscissa_of_metapost p t2)

    let direction_of_abscissa p t1 =
      S.direction_of_abscissa p (S.abscissa_of_metapost p t1)
    let point_of_abscissa p t1 =
      S.abscissa_to_point p (S.abscissa_of_metapost p t1)

    let bounding_box = S.bounding_box

    let dist_min_point path point =
      let d, a = S.dist_min_point path point in
      d, S.metapost_of_abscissa path (a)

    let dist_min_path path1 path2 =
      let d, (a1, a2) = S.dist_min_path path1 path2 in
      d, c_metapost_of_abscissa path1 path2 (a1,a2)

    let print = S.print

  end

module CTransform = Matrix

let float_of_num = Compute.num
let cpoint_of_point = Compute.point
let cpath_of_path = Compute.path
let ctransform_of_transform = Compute.transform

let baselines s = Picture_lib.baseline (Compute.picture (Types.mkPITex s))

let num_of_float = Misc.id
let point_of_cpoint = Misc.id

let path_of_cpath p =
  let knot x = Types.mkKnot Types.mkNoDir (point_of_cpoint x) Types.mkNoDir in
  let start = knot (CPath.point_of_abscissa p 0.) in
  let path = CPath.fold_left
    (fun acc _ b c d ->
       let joint = Types.mkJControls (point_of_cpoint b) (point_of_cpoint c) in
       Types.mkMPAConcat (knot d) joint acc
    ) (Types.mkMPAKnot start) p in
  if CPath.is_closed p
  then Types.mkMPACycle Types.mkNoDir Types.mkJLine path
  else Types.mkPAofMPA path

let transform_of_ctransform x = [x]

ELSE
let supported = false

let not_supported s = failwith ("Concrete."^s^" : not supported")

module CPoint =
struct
  let not_supported s = failwith ("Concrete.Cpoint."^s^" : not supported")

  type t = {x:float; y:float}

  let add _ _ = not_supported "add"
  let sub _ _ = not_supported "sub"
  let opp _ = not_supported "opp"
  let mult _ _ = not_supported "mult"
  let div _ _ = not_supported "div"

  module Infix =
  struct
    let (+/)  = add
    let (-/)  = sub
    let ( */)  = mult
    let ( //) = div
  end

  let print _ _ = not_supported "print"

end

module CPath =
struct
  let not_supported s = failwith ("Concrete.CPath."^s^" : not supported")

  type t = unit
  type abscissa = float
  type point = CPoint.t

  let length _ = not_supported "length"
  let is_closed _ = not_supported "is_closed"
  let is_a_point _ = not_supported "is_a_point"

  let intersection p1 p2 = not_supported "intersection"

  let one_intersection p1 p2 = not_supported "one_intersection"

  let reverse _ = not_supported "reverse"

  let iter _ _ = not_supported "iter"
  let fold_left _ _ = not_supported "fold_left"

  let cut_before _ _ = not_supported "cut_before"
  let cut_after _ _ = not_supported "cut_after"

  let split p t = not_supported "split"

  let subpath p t1 t2 = not_supported "subpath"

  let direction_of_abscissa p t1 = not_supported "direction_of_abscissa"
  let point_of_abscissa p t1 = not_supported "point_of_abscissa"

  let bounding_box _ = not_supported "bounding_box"

  let dist_min_point path point = not_supported "dist_min_point"
  let dist_min_path path1 path2 = not_supported "dist_min_path"

  let print _ _ = not_supported "print"

end

module CTransform =
struct
  type t =
      { xx : float; yx : float;
        xy : float; yy : float; x0 : float; y0 : float; }
end

let float_of_num _ = not_supported "float_of_num"
let compute_nums _ = not_supported "compute_nums"
let cpoint_of_point _ = not_supported "cpoint_of_point"
let cpath_of_path _ = not_supported "cpath_of_path"

let ctransform_of_transform _ = not_supported "ctransform_of_transform"

let num_of_float f = not_supported "num_of_float"
let point_of_cpoint p = not_supported "point_of_cpoint"

let path_of_cpath p = not_supported "path_of_cpath"
let transform_of_ctransform _ = not_supported "transform_of_ctransform"
let baselines p = not_supported "baselines"
END
