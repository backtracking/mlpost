type t

module Dev_save : Dviinterp.dev with type arg = bool and type cooked = t

val separe_pages : t -> t list
val get_dimen_first_page : t -> float * float * float * float
val get_bases_first_page : t -> float list

module Dev_load ( Dev : Dviinterp.dev ) : sig
  val replay : bool ->  t -> Dev.arg -> Dev.cooked
end
