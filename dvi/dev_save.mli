(** A higher-level interface of DVI files *)

(** The type of commands. All coordinates are absolute in the current page. *)
type command =
  | Rectangle of Dviinterp.info * float * float * float * float (** x,y,w,h *)
  | Glyph of Dviinterp.info * Fonts.t * Int32.t * float * float
  | Specials of Dviinterp.info * string * float *float (** s,x,y *)

(** Pages are a list of commands and information about extremal coordinates *)
type page = { c : command list;
              x_min : float;
              y_min : float;
              x_max : float;
              y_max : float;
              bases : float list
            }

(** The higher-level type of DVI documents - basically a list of pages *)
type t

val pages : t -> page list
(** get the pages of the document *)

module Dev_save : Dviinterp.dev with type arg = bool and type cooked = t
(** This is a module ready to be passed to the functor {!Dviinterp.Interp}. *)

val separate_pages : t -> t list
(** separate document into one page per document *)

val get_dimen_first_page : t -> float * float * float * float
val get_bases_first_page : t -> float list
(** get information about the first page *)

val cmd_iter : (command -> unit) -> t -> unit
(** iterate over all commands of all pages *)

(** *)
module Dev_load ( Dev : Dviinterp.dev ) : sig
  val replay : bool ->  t -> Dev.arg -> Dev.cooked
end

module Print : sig
  (* debug printing *)
  val command : Format.formatter -> command -> unit
  val page : Format.formatter -> page -> unit
  val dvi : Format.formatter -> t -> unit
end
