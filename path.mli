(* there might be a Num module later *)
type num
type point = num * num
val bp : float -> num
val pt : float -> num
val cm : float -> num
val mm : float -> num
val inch : float -> num

(* these are the functions actually related to paths *)
type t

val straight : point list -> t
val curved : point list -> t

val cycle : t -> t


(* later in the mlpost module *)
type command
type figure = command list

val draw : t -> command
