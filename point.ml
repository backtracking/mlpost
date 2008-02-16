module F = Format

module Num = struct
  type t = 
    | BP of float
    | PT of float
    | CM of float
    | MM of float
    | IN of float

  let bp f = BP f
  let pt f = PT f
  let cm f = CM f
  let mm f = MM f
  let inch f = IN f

  let print_float fmt f =
    if f = infinity then F.fprintf fmt "infinity"
    else F.fprintf fmt "%.4f" f

  let print fmt = function
    | BP f -> F.fprintf fmt "%a" print_float f 
    | PT f -> F.fprintf fmt "%apt" print_float f 
    | CM f -> F.fprintf fmt "%acm" print_float f 
    | MM f -> F.fprintf fmt "%amm" print_float f 
    | IN f -> F.fprintf fmt "%ain" print_float f 
end

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

