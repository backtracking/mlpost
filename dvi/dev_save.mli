type command =
  | Rectangle of Dviinterp.info*float * float * float * float (* x,y,w,h *)
  | Glyph of Dviinterp.info*Fonts.t * Int32.t * float * float
  | Specials of Dviinterp.info*string * float *float (* s,x,y *)

type page = { c : command list;
              x_min : float;
              y_min : float;
              x_max : float;
              y_max : float;
              bases : float list
            }

type t

val pages : t -> page list

module Dev_save : Dviinterp.dev with type arg = bool and type cooked = t

val separate_pages : t -> t list
val get_dimen_first_page : t -> float * float * float * float
val get_bases_first_page : t -> float list

val cmd_iter : (command -> unit) -> t -> unit

module Dev_load ( Dev : Dviinterp.dev ) : sig
  val replay : bool ->  t -> Dev.arg -> Dev.cooked
end
