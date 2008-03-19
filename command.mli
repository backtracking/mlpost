(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Johannes Kanig, Stephane Lescuyer                       *)
(*  and Jean-Christophe Filliatre                                         *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2, with the special exception on linking              *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(** General Commands to build figures *)


type t = Types.command
(** the abstract commands type *)

type figure = t list
(** a figure is a list of commands *)


(** {2 Drawing Commands} *)

val draw : ?color:Color.t -> ?pen:Pen.t -> ?dashed:Dash.t -> Path.t -> t
(** draw a path 
  @param color the color of the path; default is black
  @param pen the pen used to draw the path; default is {!Pen.default}
  @param dashed if given, the path is drawn using that dash_style. *)

val draw_arrow : ?color:Color.t -> ?pen:Pen.t -> ?dashed:Dash.t -> Path.t -> t
(** draw a path with an arrow head; the optional arguments are the same as for
   {!draw} *)

val fill : ?color:Color.t -> Path.t -> t
(** fill a contour given by a closed path 
  @param color the color used to fill the area; default is black *)

val draw_box : ?fill:Color.t -> Box.t -> t
(** draw a box 
  @param fill the color used to fill the box *)

val draw_pic : Picture.t -> t
(** draws a picture *) 

(** {2 Manipulating Commands} *)

val iter : int -> int -> (int -> t list) -> t
(** [iter m n f] builds a command that corresponds to the sequence
  f m; f (m+1); ... ; f(n) of commands *)

val append : t -> t -> t
(** append two commands to form a compound command *)

val (++) : t -> t -> t
(** abbreviation for [append] *)
val seq : t list -> t
(** group a list of commands to a single command *)

(** {2 Labels} *)

(** Positions; useful to place labels *)
type position = Types.position =
  | Pcenter
  | Pleft
  | Pright
  | Ptop
  | Pbot
  | Pupleft
  | Pupright
  | Plowleft
  | Plowright

(** [label ~pos:Pleft pic p] puts picture [pic] at the left of point [p] *)
val label : ?pos:position -> Picture.t -> Point.t -> t

(** works like [label], but puts a dot at point [p] as well *)
val dotlabel : ?pos:position -> Picture.t -> Point.t -> t
