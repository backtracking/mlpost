type transform = Matrix.t
type num = float
type dash = point * num list
type pen = transform option
type color = Types.color

type path = Spline_lib.path

type tex = Gentex.t

type command = private
  | Tex of tex
  | Stroke_path of path * color * pen * dash
  | Fill_path of path * color
  | Clip of picture * path
  | ExternalImage of string * spec_image

type t

type id = int

val content : t -> command list

val tex : Gentex.t -> t
val fill_path : path -> color -> t
val stroke_path : path -> color -> pen -> dash -> t
val clip : t -> path -> t
val external_image : string -> spec_image -> t
val interative : Spline_lib.path -> id -> t

val on_top : t -> t -> t

val transform : t -> Matrix.t -> t
val shift : t -> float -> float -> t
val bounding_box : t -> float * float * float * float




module Cairo :
sig
  val draw : Cairo.t -> t -> unit
  val where : Cairo.t -> t -> float * float -> id
  val move : Cairo.t -> t -> id -> Point.t -> Point.t
end
