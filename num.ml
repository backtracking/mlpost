module F = Format

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
