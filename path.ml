type num = 
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

type point = 
  | Ppair of num * num
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

type direction = 
  | Vec of point
  | Curl of float
  | NoDir 

type joint = 
    JLine
  | JCurve
  | JCurveNoInflex
  | JTension of float * float
  | JControls of point * point

type knot = direction * point * direction

(* the intention is to add new knots in front,
 * i. e. the list has to be reversed for printing *)
type t' = knot * (joint * knot) list
type t = { path: t'; cycle: joint option}

let start k = { path = k , [] ; cycle = None }

let concat ({path = (k',l); cycle = c} as p) j k =
  { p with path=k', ((j,k) :: l) }

let cycle j p = {p with cycle = Some j}

let append {path = (k1,l1); cycle = c1} j {path = (k2,l2);  cycle = _} =
  { path = (k1, l1@( (j,k2)::l2) ) ; cycle = c1}


(* later in the mlpost module *)
type command = 
  | Draw of t

type figure = command list

let draw t = Draw t
