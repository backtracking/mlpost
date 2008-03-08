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

type t = N of string * t list

(* smart constructors *)

val leaf : string -> t
val node : string -> t list -> t
val bin : string -> t -> t -> t

(** 2. Drawing *)

type node_style = Circle | Rect

type arrow_style = Directed | Undirected

val draw : 
  ?scale:(float -> Num.t) -> 
  ?node_style:node_style -> ?arrow_style:arrow_style -> 
  ?ls:float -> ?nw:float -> ?cs:float -> 
  t -> Command.figure
  (** Default scale is [Num.cm]. Drawing parameters are:
      - [ls] (level sep): vertical distance between levels; 
        default value is 1.0.
      - [nw] (node width): width of one node; default value is 0.5
      - [cs] (children sep): horizontal distance between siblings; 
        default value is 0.2
  *)

