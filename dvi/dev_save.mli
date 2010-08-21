(** A higher-level interface of DVI files *)

(** The type of commands. All coordinates are absolute in the current page. *)
type command = Dviinterp.command


(** The higher-level type of DVI documents - basically a list of pages *)
type t = command list list
type page = command list

val get_dimen : page -> (float * float * float * float)
(* Warning not in cm *)

module Print : sig
  (* debug printing *)
  val command : Format.formatter -> command -> unit
  val page : Format.formatter -> page -> unit
  val dvi : Format.formatter -> t -> unit
end
