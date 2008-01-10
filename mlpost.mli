type point = float * float
type path
type command
type figure = command list

val draw : path -> command
val straight : point list -> path

val cm : float -> float
val mm : float -> float
val pt : float -> float
val inch : float -> float
