type num
type point = num * num
type path
type command
type figure = command list

val draw : path -> command

val straight : point list -> path
val curved : point list -> path

val cycle : path -> path

val bp : float -> num
val pt : float -> num
val cm : float -> num
val mm : float -> num
val inch : float -> num
