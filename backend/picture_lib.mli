type point = Point.t
type transform = Matrix.t
type num = float
type dash = point * num list
type pen = transform option
type color = Types.color

type path = Spline_lib.path

type tex = Gentex.t

type commands = 
    private
  | Empty
  | Transform of transform * commands
  | OnTop of commands list
  | Tex of tex
  | Stroke_path of path * color * pen * dash
  | Fill_path of path * color
  | Clip of commands  * path
  | ExternalImage of string * float * float

type t

type id = int

val content : t -> commands

val tex : Gentex.t -> t
val fill_path : path -> color -> t
val stroke_path : path -> color -> pen -> dash -> t
val clip : t -> path -> t
val external_image : string -> 
  [< `Exact of float * float
  | `Height of float
  | `Inside of float * float
  | `None
  | `Width of float ] -> t
val interative : Spline_lib.path -> id -> t

val on_top : t -> t -> t

val transform : t -> Matrix.t -> t
val shift : t -> float -> float -> t
val bounding_box : t -> point * point




module Cairo :
sig
  val draw : Cairo.t -> t -> unit
  val where : Cairo.t -> t -> float * float -> id list
  val move : Cairo.t -> t -> id -> Point.t -> Point.t
end
