module F = Format

type t = 
  | Ppair of Num.t * Num.t
  | Dir of float
  | Up
  | Down
  | Left
  | Right

let p (a,b) = Ppair (a,b)
let dir f = Dir f
let up = Up
let down = Down
let left = Left
let right = Right

let print fmt = function
  | Up -> F.fprintf fmt "up"
  | Down -> F.fprintf fmt "down"
  | Left -> F.fprintf fmt "left"
  | Right -> F.fprintf fmt "right"
  | Dir d -> F.fprintf fmt "dir %a" Num.print_float d
  | Ppair (m,n) -> F.fprintf fmt "(%a,%a)" Num.print m Num.print n

