type t
val transform : Transform.t -> t -> t
val default : t
val circle : t
val square : t
val from_path : Path.t -> t
val print : Format.formatter -> t -> unit
