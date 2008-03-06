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

open Path
open Point
open Num
open Command
module SP = SimplePath

(*  puts labels at given points with given text *)
let dotlabels ?(pos=Center) ls lp =
  List.map2 (fun s p -> dotlabel ~pos:pos (Picture.tex s) p) ls lp

let draw_simple_arrow ?color ?pen ?(style=JLine) a b =
  Command.draw_arrow ?color ?pen
    (SP.jointpathp [a;b] [style])

let box_simple_arrow ?color ?pen ?(style=JLine) a b =
  Command.draw_arrow ?color ?pen
    (let p = 
      SP.jointpathp [Box.center a; Box.center b] [style]
      in
     cut_after (bpath b) (cut_before (bpath a) p))
