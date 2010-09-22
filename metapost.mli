type job = File.t * Types.commandpic
type jobs = job list

(** A queue of jobs; TODO move this to another module *)
val figures : job Queue.t
val emited : unit -> jobs
val emit : string -> Types.commandpic -> unit

val generate :
  ?prelude:string -> ?verbose:bool ->
  ?clean:bool -> string -> (string * Types.commandpic) list -> unit

(** Generate files of corresponding type, using the argument of type [jobs],
   and return information about the created files *)
val mp : string -> ?prelude:string -> jobs -> File.t * File.t Misc.IntMap.t
val mps : ?prelude:string -> ?verbose:bool -> string -> jobs -> File.t list
val pdf : ?prelude:string -> ?verbose:bool -> string -> jobs -> File.t list
val png : ?prelude:string -> ?verbose:bool -> string -> jobs -> File.t list

(** Same as above, but use a temporary directory *)
val temp_mp :
  ?prelude:string -> ?verbose:bool -> ?clean:bool -> string -> jobs ->
    File.t * File.t Misc.IntMap.t
val temp_mps :
  ?prelude:string -> ?verbose:bool -> ?clean:bool -> string ->
    jobs -> File.t list
val temp_pdf :
  ?prelude:string -> ?verbose:bool -> ?clean:bool -> string ->
    jobs -> File.t list
val temp_png :
  ?prelude:string -> ?verbose:bool -> ?clean:bool -> string ->
    jobs -> File.t list

(** Same as above, but use a temporary directory and take jobs from the job
   queue *)
val dump : ?prelude:string -> ?verbose:bool -> ?clean:bool -> string -> unit
val dump_mp : ?prelude:string -> ?verbose:bool -> ?clean:bool -> string -> unit
val dump_mps : ?prelude:string -> ?verbose:bool -> ?clean:bool -> string -> unit
val dump_png : ?prelude:string -> ?verbose:bool -> ?clean:bool -> string -> unit
val dump_pdf : ?prelude:string -> ?verbose:bool -> ?clean:bool -> string -> unit

val read_prelude_from_tex_file : string -> string

val dump_tex : ?prelude:string -> string -> unit

val slideshow : Types.commandpic list -> int -> (int * Types.commandpic) list
val emit_slideshow : string -> Types.commandpic list -> unit

val dumpable : unit -> unit
val depend : string -> unit

val set_prelude : string -> unit
val set_filename_prefix : string -> unit
val set_required_files : string list -> unit
