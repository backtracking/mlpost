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

(** Diagrams. *)

(** 1. Creation *)

type node

val node : float -> float -> string -> node

type t

val create : node list -> t

type dir = Up | Down | Left | Right | Angle of float

val arrow : 
  t -> ?lab:string -> ?pos:Command.position -> 
  ?outd:dir -> ?ind:dir -> node -> node -> unit

(** 2. Drawing *)

type node_style = Circle | Rect

val draw : 
  ?scale:(float -> Num.t) -> ?style:node_style -> 
  ?fill:Color.t -> ?stroke:Color.t -> ?pen:Pen.t ->
  t -> Command.figure
  (** default scale is 40bp *)

