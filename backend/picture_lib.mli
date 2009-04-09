type point = Point_lib.t
type transform = Matrix.t
type num = float
type dash = float * num list
type pen = transform
type color = Types.color

type path = Spline_lib.path

type tex = Gentex.t

type commands = 
    private
  | Empty
  | Transform of transform * commands
  | OnTop of commands list
  | Tex of tex
  | Stroke_path of path * color option * pen option * dash option 
  | Fill_path of path * color option
  | Clip of commands  * path
  | ExternalImage of string * float * float

type t

type id = int

val content : t -> commands

val tex : Gentex.t -> t
val fill_path : path -> color option -> t
val stroke_path : path -> color option -> pen option -> dash option -> t
val draw_point : point -> t
val clip : t -> path -> t
val external_image : string -> 
  [< `Exact of float * float
  | `Height of float
  | `Inside of float * float
  | `None
  | `Width of float ] -> t
val interative : Spline_lib.path -> id -> t

val on_top : t -> t -> t

val empty : t
val transform : Matrix.t -> t -> t
val shift : t -> float -> float -> t
val bounding_box : t -> point * point




module Cairo :
sig
  val draw : Cairo.t -> t -> unit
  val where : Cairo.t -> t -> float * float -> id list
  val move : Cairo.t -> t -> id -> Point.t -> Point.t
end


module Dash :
  sig
    type t = dash
    type input_dash =
      | On of float
      | Off of float

    val shifted : float -> t -> t
    val line : t
    val dots : t
    val pattern : input_dash list -> t
    val scale : float -> t -> t
end
