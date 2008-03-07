
(* trees *)

type t = N of string * t list

(* smart constructors *)

val leaf : string -> t
val node : string -> t list -> t
val bin : string -> t -> t -> t

(* drawing *)

val draw : t -> Command.figure

