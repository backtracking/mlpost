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

open Command
open Box
open Color


let rec mklegend ensstroke colstroke fill vb1 vb2 = function
    |[] -> set_fill fill (set_stroke ensstroke (
	box (hbox ~padding:(Num.bp 10.) 
	       [vbox ~padding:(Num.bp 10.) (List.rev vb1);
		vbox ~padding:(Num.bp 10.) (List.rev vb2)])))
    |(col,text)::res -> 
       let c = set_fill col (set_stroke colstroke (
			       empty ~width:(Num.bp 20.) ~height:(Num.bp 10.) ())) 
       in
	 mklegend ensstroke colstroke fill (c::vb1) (tex text::vb2) res


let legend ?ensstroke ?colstroke ?fill l =
  let ensstroke = match ensstroke with
    |None-> Color.white
    |Some i -> i
  in
  let colstroke = match colstroke with
    |None-> Color.white
    |Some i -> i
  in
  let fill = match fill with
    |None-> Color.white
    |Some i -> i
  in
    Picture.make (Box.draw (mklegend ensstroke colstroke fill [] [] l))

