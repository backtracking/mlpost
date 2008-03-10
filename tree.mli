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

(* trees *)

(** 1. Creation *)

type t
    (** the abstract type of trees *)

type node_style = Circle | Rect

val leaf : ?style:node_style -> ?fill:Color.t -> string -> t
val node : ?style:node_style -> ?fill:Color.t -> string -> t list -> t
val bin  : ?style:node_style -> ?fill:Color.t -> string -> t -> t -> t

(** 2. Drawing *)

type arrow_style = Directed | Undirected

val draw : 
  ?scale:(float -> Num.t) -> 
  ?node_style:node_style -> ?arrow_style:arrow_style -> 
  ?fill:Color.t -> ?stroke:Color.t -> ?pen:Pen.t ->
  ?ls:float -> ?nw:float -> ?cs:float -> 
  t -> Command.figure
  (** Default scale is [Num.cm]. 
      Drawing parameters are:
      - [ls] (level sep): vertical distance between levels.
        The default value is 1.0. A negative value draws the tree upward.
      - [nw] (node width): width of one node. The default value is 0.5.
      - [cs] (children sep): horizontal distance between siblings.
        The default value is 0.2.
  *)

