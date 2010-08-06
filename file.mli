module LowLevel : sig
  val move : string -> string -> unit
  val copy : string -> string -> unit
  val rmdir : string -> unit

  val read_from : string -> (in_channel -> 'a) -> 'a

  val write_to : string -> (out_channel -> 'a) -> 'a
  val write_to_formatted : string -> (Format.formatter -> 'a) -> 'a
end

module Dir : sig
  type t
  val compare : t -> t -> int

  val rm : t -> unit
  val concat : t -> t -> t
  val from_string : string -> t


  val mk : t -> int -> unit

  val temp : t
  val empty : t

  val ch : t -> unit
  val cwd : unit -> t

end

type t
(** a file name, including directory information *)

val from_string : string -> t

val to_string : t -> string

val place : Dir.t -> t -> t
(** replace the current directory information of the file by the one given *)

val concat : Dir.t -> t -> t
(** concat directory information given to the one of the file *)

val append : t -> string -> t
(** append string to file name - do not use this for file extensions *)

val move : t -> t -> unit
(** move a file to another place *)

val read_from : t -> (in_channel -> 'a) -> 'a

val compare : t -> t -> int

val basename : t -> string
val extension : t -> string option
val dir : t -> Dir.t

val clear_dir : t -> t

val set_ext : t -> string -> t
(** clear extension if passed empty string *)

val write_to : t -> (out_channel -> 'a) -> 'a
val write_to_formatted : t -> (Format.formatter -> 'a) -> 'a

module Map : Map.S with type key = t
