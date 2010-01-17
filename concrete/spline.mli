type point = Ctypes.point
type abscissa = float
type t

val inter_depth : int ref
val debug : bool

val print : Format.formatter -> t -> unit

val create : 
  ?min:float -> ?max:float -> point -> point -> point -> point -> t

val create_with_offset : 
  float -> point -> point -> point -> point -> t

val min : t -> abscissa
val max : t -> abscissa

val explode : t -> point * point * point * point

val set_min_max : (float -> float) -> (float -> float) -> t -> t

val left_point : t -> point
val right_point : t -> point
val right_control_point : t -> point

val reverse : (float -> float) -> t -> t

val s_of_01 : t -> abscissa -> abscissa
val _01_of_s : t -> abscissa -> abscissa
val point_of_s : t -> abscissa -> point

val direction : t -> abscissa -> point

val bounding_box : t -> float * float * float * float
val precise_bounding_box : t -> float * float * float * float

val one_intersection : t -> t -> float * float
val intersection : t -> t -> (float * float) list
(** raise Not_found if there is no intersection *)

val apply4 : (point -> point -> point -> point -> 'a) -> t -> 'a

type split = 
  | Min
  | Max
  | InBetween of t * t

val split : t -> abscissa -> split

val dist_min_point : point -> (float * float) -> t -> float * float
val dist_min_path : 
  (float * (float * float)) -> t -> t -> float * (float * float)

val translate : point -> t -> t
val transform : Matrix.t -> t -> t
