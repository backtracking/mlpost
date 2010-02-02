(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Johannes Kanig, Stephane Lescuyer                       *)
(*  Jean-Christophe Filliatre, Romain Bardou and Francois Bobot           *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

type transform = Matrix.t
type num = float
type dash = float * num list
type pen = transform
type color = Types.color

type path = Spline_lib.path

type interactive
type commands = 
    private
  | Empty
  | Transform of transform * commands
  | OnTop of commands list
  | Tex of Gentex.t
  | Stroke_path of path * color option * pen * dash option 
  | Fill_path of path * color option
  | Clip of commands  * path
  | ExternalImage of string * float * float

type size

type t

type id = int

val content : t -> commands

val tex : Gentex.t -> t
val fill_path : path -> color option -> t
val stroke_path : path -> color option -> pen -> dash option -> t
val draw_point : Point_lib.t -> t
val default_line_size : float

val clip : t -> path -> t
val external_image : string -> 
  [< `Exact of float * float
  | `Height of float
  | `Inside of float * float
  | `None
  | `Width of float ] -> t
val interative : Spline_lib.path -> id -> t

val on_top : t -> t -> t

val empty : t
val transform : Matrix.t -> t -> t
val shift : t -> float -> float -> t
val bounding_box : t -> Point_lib.t * Point_lib.t

(* Return the empty list if the picture is not directly a Tex *)
val baseline : t -> float list



module Dash :
  sig
    type t = dash
    type input_dash =
      | On of float
      | Off of float

    val shifted : float -> t -> t
    val line : t
    val dots : t
    val pattern : input_dash list -> t
    val scale : float -> t -> t
end
