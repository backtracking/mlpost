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

type t

val draw : ?color:Color.t -> ?pen:Pen.t -> Path.t -> t
val draw_arrow : ?color:Color.t -> ?pen:Pen.t -> Path.t -> t
val fill : ?color:Color.t -> Path.t -> t
val iter : int -> int -> (int -> t list) -> t
val draw_box : ?fill:Color.t -> Box.t -> t

val append : t -> t -> t
val (++) : t -> t -> t

type position =
  | Pcenter
  | Pleft
  | Pright
  | Ptop
  | Pbot
  | Pupleft
  | Pupright
  | Plowleft
  | Plowright

val label : ?pos:position -> Picture.t -> Point.t -> t
val dotlabel : ?pos:position -> Picture.t -> Point.t -> t

type figure = t list
val print : int -> Format.formatter -> figure -> unit

