type point = Ctypes.point
type abscissa = float

type t
(** The type of Splines *)

val inter_depth : int ref
(** A mesure to decide how many iterations do to in intersection computations;
 * higher means more precise *)
val debug : bool

val print : Format.formatter -> t -> unit

val create : 
  ?min:float -> ?max:float -> point -> point -> point -> point -> t
  (** [create a b c d] creates a spline with points a and d and control points b
    and c. By default, the abscissa of the spline starts at [0.] and ends at
    [1.].
   @param min give minimal abscissa
   @param max give maximal abscissa
   *)

val create_with_offset : float -> point -> point -> point -> point -> t
(** create a spline with abscissa between [ [f,f+1] ] *)

val min : t -> abscissa
(** minimal abscissa *)
val max : t -> abscissa
(** maximal abscissa *)

val explode : t -> point * point * point * point
(** return the four points of the spline; left point, left control point, second
   point, second control point*)

val set_min_max : (float -> float) -> (float -> float) -> t -> t
(** set the minimal and maximal abscissa using two functions. *)

val left_point : t -> point
val left_control_point : t -> point
val right_point : t -> point
val right_control_point : t -> point
(** the four points of a spline *)

val reverse : (float -> float) -> t -> t
(** reverse a spline, using a conversion function for max and min *)

val _01_of_s : t -> abscissa -> abscissa
(** take an abscissa on spline [s], and compute its position as if 
    minimal and maximal abscissa of the spline were 0. and 1. *)
  
val s_of_01 : t -> abscissa -> abscissa
(** inverse of _01_of_s: take an abscissa between 0. and 1. and return the
   corresponding abscissa of the spline, taking into account its minimal and
   maximal abscissa *)

val point_of : t -> abscissa -> point
(** compute the location of the given abscissa on a spline *)

val point_of_s : t -> abscissa -> point
(** compute the location of the given abscissa on a spline, but convert abscissa
 * to [0,1] interval first *)

val direction : t -> abscissa -> point
(** give the direction (derivative) of the spline at the given abscissa *)

val bounding_box : t -> float * float * float * float
(** a bounding_box of the given spline *)

val precise_bounding_box : t -> float * float * float * float
(** a more precise bounding_box of the given spline *)

val one_intersection : t -> t -> float * float
(** compute a single intersection of the two splines; raise [Not_found] if there
 * is no intersection. *)

val intersection : t -> t -> (float * float) list
(** compute all intersections of the two splines; raise [Not_found] if there
 * is no intersection. *)

val apply4 : (point -> point -> point -> point -> 'a) -> t -> 'a
(** apply a function to the four points of the spline *)

type split = 
  | Min
  | Max
  | InBetween of t * t
  (** the type which caracterizes a split of a spline - 
     Min - we have splitted at the left end
     Max - we have splitted at the right end
     InBetween (s1,s2) - we have splitted somewhere in between, and the
     resulting two new splines are [s1] and [s2]
   *)

val split : t -> abscissa -> split
(** split a spline at the given abscissa *)

val dist_min_point : point -> (float * float) -> t -> float * float
(** compute the minimal distance of a point with a spline; return this distance
 * if it is smaller than the given distance *)

val dist_min_path : 
  (float * (float * float)) -> t -> t -> float * (float * float)
(** compute the minimal distance of two splines; return this distance
 if it is smaller than the given distance *)

val translate : point -> t -> t
(** translate all points of the spline *)

val transform : Matrix.t -> t -> t
(** transform all points of the spline *)
