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

(** the Convenience Module *)

val draw : 
    ?style:Path.joint -> ?cycle:Path.joint -> ?scale:(float -> Num.t) ->
      ?color:Color.t -> ?pen:Pen.t -> (float * float) list -> Command.t
(** a convenient method to draw a simple path
  @param style the joint style used for all joints of the path
  @param cycle if given, the path is closed using the given style
  @param scale permits to scale the whole path
  @param color permits to give a color to draw the path; default is black
  @param pen the pen to draw the path *)
