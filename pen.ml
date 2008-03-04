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
