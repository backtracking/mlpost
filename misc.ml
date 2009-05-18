(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Johannes Kanig, Stephane Lescuyer                       *)
(*  and Jean-Christophe Filliatre                                         *)
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

let write_to_file filename f =
  let chan = open_out filename in
    f chan;
    close_out chan

let write_to_formatted_file filename f =
  write_to_file filename
    (fun chan ->
      let fmt = Format.formatter_of_out_channel chan in
        f fmt)

let print_option start printer fmt = function
  | None -> ()
  | Some o -> Format.fprintf fmt "%s%a " start printer o

let rec print_list sep prf fmt = function
  | [] -> ()
  | [x] -> prf fmt x
  | (x::xs) -> prf fmt x; sep fmt (); print_list sep prf fmt xs

let space fmt () = Format.fprintf fmt "@ "
let comma fmt () = Format.fprintf fmt ",@ "
let semicolon fmt () = Format.fprintf fmt ";@ "

let rec fold_from_to f acc a b =
  if a <= b then fold_from_to f (f acc a) (a+1) b else acc

let sprintf s =
  let buf = Buffer.create 1024 in
  let fmt = Format.formatter_of_buffer buf in
  Format.kfprintf 
    (fun _ -> Format.pp_print_flush fmt (); Buffer.contents buf) fmt s

(*Filename.generic_quote*)
let generic_quote whatquote quotequote s =
  let l = String.length s in
  let b = Buffer.create (l + 20) in
  for i = 0 to l - 1 do
    if s.[i] = whatquote
    then Buffer.add_string b quotequote
    else Buffer.add_char b  s.[i]
  done;
  Buffer.contents b  



(* persistent queues *)
module Q = struct

  type 'a t = 'a list * 'a list

  exception Empty

  let empty = [], []

  let push x (i, o) = (x :: i, o)

  let pop = function
    | [], [] -> raise Empty
    | (i, x :: o) -> x, (i, o)
    | (i, []) -> match List.rev i with
	| x :: o -> x, ([], o)
	| [] -> assert false

  let of_list l =
    List.fold_left (fun q c -> push c q) empty l

end

  

