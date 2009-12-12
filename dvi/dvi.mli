
type command =
  | SetChar of int32
  | SetRule of int32 * int32
  | PutChar of int32
  | PutRule of int32 * int32
  | Push
  | Pop
  | Right of int32
  | Wdefault
  | W of int32
  | Xdefault
  | X of int32
  | Down of int32
  | Ydefault
  | Y of int32
  | Zdefault
  | Z of int32
  | FontNum of int32
  | Special of string

type page 

type t

val get_conv : t -> float

val fontmap : t -> Fonts.font_def Dvi_util.Int32Map.t

val commands : page -> command list

val pages : t -> page list

val read_file : string -> t
