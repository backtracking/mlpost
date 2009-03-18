type t
val draw : Cairo.t -> t -> unit
val create : string -> string list -> t list
val get_dimen_pt : t -> float * float * float * float
val get_dimen_cm : t -> float * float * float * float
val get_bases_pt : t -> float list
val get_bases_cm : t -> float list
(** donne la dimension en centimètre *)
