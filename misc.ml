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

let write_to_file filename f =
  let chan = open_out filename in
    f chan;
    close_out chan

let write_to_formatted_file filename f =
  write_to_file filename
    (fun chan ->
      let fmt = Format.formatter_of_out_channel chan in
        f fmt)

let pi = 3.14159
let deg2rad f = pi *. f /. 180.

let print_option start printer fmt = function
  | None -> ()
  | Some o -> Format.fprintf fmt "%s%a " start printer o

let rec print_list sep prf fmt = function
  | [] -> ()
  | [x] -> prf fmt x
  | (x::xs) -> prf fmt x; sep fmt (); print_list sep prf fmt xs

let space fmt () = Format.fprintf fmt "@ "
let comma fmt () = Format.fprintf fmt ",@ "
