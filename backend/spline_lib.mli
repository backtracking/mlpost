type point = Cairo.point
type path
type abscissa = float

val inter_depth : int ref
val debug : bool ref


val create : point -> point -> point -> point -> path
  (** create a b c d return a path with :
      - point a as the starting point, 
      - point b as its control point,
      - point d as the ending point,
      - point c as its control point *)

val close : path -> path

val min_abscissa : path -> abscissa
val max_abscissa : path -> abscissa
  

val add_end : path -> point -> point -> path
  (** add_end p a b return the path p with one more spline at the end.*)
  
val reverse : path -> path
  (** reverse p return the path p reversed *)
  
val union : path -> path -> path
  (** union p1 p2 return the union of path p1 and p2. [min_abscissa p1;max_abscissa p1] 
      are points of p1, ]max_abscissa p1;max_abscissa p1+max_abscissa p2-min_abscissa p2]
      are points of p2 *)

val union_conv : path -> path -> (abscissa -> abscissa)
  
val one_intersection : path -> path -> (abscissa * abscissa)

val intersection : path -> path -> (abscissa * abscissa) list
  (** intersection p1 p2 return a list of pair of abscissa. In each pairs 
      (a1,a2), a1 (resp. a2) is the abscissa in p1 (resp. p2) of one 
      intersection point between p1 and p2. Additionnal point of intersection 
      (two point for only one real intersection) can appear in degenerate case. *)

val fold_left : ('a -> point -> point -> point -> point -> 'a) 
  -> 'a -> path -> 'a
  (** fold on all the splines of a path *)

val iter : (point -> point -> point -> point -> unit) -> path -> unit
  (** iter on all the splines of a path *)

val cut_before : path -> path -> path
val cut_after : path -> path -> path
  (** remove the part of a path before the first intersection 
      or after the last*)

val split : path -> abscissa -> path * path
val subpath : path -> abscissa -> abscissa -> path

val abscissa_to_point : path -> abscissa -> point
val bounding_box : path -> point * point
val unprecise_bounding_box : path -> point * point
val printf : Format.formatter -> path -> unit
val dist_min_point : path -> point -> abscissa
val dist_min_path : path -> path -> abscissa * abscissa

val translate : path -> point -> path
val transform : path -> Cairo.matrix -> path

val buildcycle : path list -> path

module Epure :
sig
  type t
    
  val create : path -> t
  val union : t -> t -> t
  val transform : t -> Cairo.matrix -> t
  val bounding_box : t -> point * point
end

module Approx =
  sig
    val lineto : point list -> path
    val fullcircle : unit -> path
    val halfcirle : unit -> path
    val quartercircle : unit -> path
  end

module Cairo =
  sig
    val draw : Cairo.t -> path -> unit
  end

module Metapath =
 sig
   type t

   type joint
   type knot
   type direction
   val knot : direction -> point -> direction -> knot
   
   val vec_direction : point -> direction
   val curl_direction : float -> direction
   val no_direction : direction
   
   val line_joint : joint
   val curve_joint : joint
   val curve_no_inflex_joint : joint
   val tension_joint : float -> float -> joint
   val controls_joint : point -> point -> joint
   

   val concat : t -> joint -> knot -> t
   val append : t -> joint -> t -> t
   val cycle : direction -> joint -> t -> path

   val to_path : t -> path
   val from_path : path -> t
 end


     

