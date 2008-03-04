
open Format

type t = 
  | Tex of string
(* later in the mlpost module *)

let tex s = Tex s

let print fmt = function
  | Tex s -> fprintf fmt "btex %s etex" s
