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

val draw : 
    ?style:Path.joint -> ?cycle:Path.joint -> ?scale:(float -> Num.t) ->
      ?color:Color.t -> ?pen:Pen.t -> (float * float) list -> Command.t

val path : 
    ?style:Path.joint -> ?cycle:Path.joint -> ?scale:(float -> Num.t) -> 
      (float * float) list -> Path.t

val jointpath : 
    ?scale:(float -> Num.t) -> (float * float) list -> Path.joint list -> Path.t

val p :
    ?l:Path.direction -> ?r:Path.direction -> 
      ?scale:(float -> Num.t) -> float * float -> Path.knot
