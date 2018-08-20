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

  val draw_tex : Cairo.t -> Gentex.t -> unit

module MetaPath :
  sig
    type pen = Matrix.t
    val stroke : Cairo.t -> pen -> Spline_lib.path -> unit
    val fill : Cairo.t -> Spline_lib.path -> unit
    val draw_path : Cairo.t -> Spline_lib.path -> unit
  end

module Picture :
sig
  val draw : Cairo.t -> float -> float -> Picture_lib.t -> unit
  val where : Cairo.t -> Picture_lib.t -> float * float -> Picture_lib.id list
  val move :
    Cairo.t -> Picture_lib.t -> Picture_lib.id -> float * float -> float * float
end

