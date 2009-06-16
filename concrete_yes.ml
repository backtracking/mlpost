# 2 "concrete_yes.ml"
let supported = true

type cnum = float

module CPoint = Point_lib

module CPath = 
  struct
    module S = Spline_lib
    type t = S.path
    type abscissa = float
    type point = CPoint.t
    
    let length = S.metapost_length
    let is_closed = S.is_closed
    let is_a_point = S.is_a_point

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

    let direction_of_abscissa p t1 = S.direction_of_abscissa p (S.abscissa_of_metapost p t1)
    let point_of_abscissa p t1 = S.abscissa_to_point p (S.abscissa_of_metapost p t1)

    let bounding_box = S.bounding_box

    let dist_min_point path point = S.metapost_of_abscissa path (S.dist_min_point path point)
    let dist_min_path path1 path2 = c_metapost_of_abscissa path1 path2 (S.dist_min_path path1 path2)

    let print = S.print

  end

let float_of_num = LookForTeX.num
let cpoint_of_point = LookForTeX.point
let cpath_of_path = LookForTeX.path


let num_of_float f = Types.mkF f
let point_of_cpoint p = 
  let x = Types.mkF p.CPoint.x in
  let y = Types.mkF p.CPoint.y in
  Types.mkPTPair x y

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