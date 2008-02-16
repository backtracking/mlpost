module F = Format
module Num = Point.Num

type direction = 
  | Vec of Point.t
  | Curl of float
  | NoDir 

type joint = 
    JLine
  | JCurve
  | JCurveNoInflex
  | JTension of float * float
  | JControls of Point.t * Point.t

type knot = direction * Point.t * direction

(* the intention is to add new knots in front,
 * i. e. the list has to be reversed for printing *)
type t' = knot * (joint * knot) list
type t = { path: t'; cycle: joint option}

let start k = { path = k , [] ; cycle = None }

let concat ({path = (k',l); cycle = c} as p) j k =
  { p with path=k, ((j,k') :: l) }

let cycle j p = {p with cycle = Some j}

let append {path = (k1,l1); cycle = c1} j {path = (k2,l2);  cycle = _} =
  { path = (k2, l2@( (j,k1)::l1) ) ; cycle = c1}

let print_joint fmt = function
  | JLine -> F.fprintf fmt "--"
  | JCurve -> F.fprintf fmt ".."
  | JCurveNoInflex -> F.fprintf fmt "..."
  | JTension (a,b) -> F.fprintf fmt "..tension %a and %a .." Num.print_float a Num.print_float b
  | JControls (a,b) -> F.fprintf fmt "..controls %a and %a .." Point.print a Point.print b

let print_dir fmt = function
  | NoDir -> ()
  | Vec p -> F.fprintf fmt "{%a}" Point.print p
  | Curl f -> F.fprintf fmt "{curl %a}" Num.print_float f

let print_knot fmt (d1,p,d2) = 
  F.fprintf fmt "%a%a%a" print_dir d1 Point.print p print_dir d2

let print_subpath fmt (k,l) =
  let rec print_jklist fmt = function
    | [] -> ()
    | (j,k) :: rl ->
        F.fprintf fmt "%a %a %a" print_jklist rl print_knot k print_joint j 
  in
    F.fprintf fmt "%a %a" print_jklist l print_knot k

let print fmt {path = p; cycle = c } =
  match c with
    | None -> print_subpath fmt p
    | Some j -> 
        F.fprintf fmt "%a %a cycle" print_subpath p print_joint j

