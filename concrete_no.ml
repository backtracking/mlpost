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

# 2 "concrete_no.ml"
let supported = false

let not_supported s = failwith ("Concrete."^s^" : not supported")

let set_prelude filename = not_supported "set_prelude"

let set_t1disasm opt = not_supported "set_t1disasm"

let set_prelude2 prelude = not_supported "set_prelude2"

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

let float_of_num _ = not_supported "float_of_num"
let cpoint_of_point _ = not_supported "cpoint_of_point"
let cpath_of_path _ = not_supported "cpath_of_path"


let num_of_float f = not_supported "num_of_float"
let point_of_cpoint p = not_supported "point_of_cpoint"

let path_of_cpath p = not_supported "path_of_cpath"
