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

module F = Format

type t = 
  | PenCircle
  | PenSquare
  | FromPath of Path.t
  | Transformed of t * Transform.t

let transform tr = function
  | Transformed (t,tr') -> Transformed (t,tr'@tr)
  | _ as x -> Transformed (x,tr)

let default = Transformed (PenCircle, [Transform.scaled 0.5])
let circle = PenCircle
let square = PenSquare
let from_path p = FromPath p

let rec print fmt = function
  | PenCircle -> F.fprintf fmt "pencircle"
  | PenSquare -> F.fprintf fmt "pensquare"
  | FromPath p -> F.fprintf fmt "makepen (%a)" Path.print p
  | Transformed (p,tr) -> F.fprintf fmt "%a %a" print p Transform.print tr
