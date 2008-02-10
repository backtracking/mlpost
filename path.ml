module F = Format

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
  { p with path=k, ((j,k') :: l) }

let cycle j p = {p with cycle = Some j}

let append {path = (k1,l1); cycle = c1} j {path = (k2,l2);  cycle = _} =
  { path = (k2, l2@( (j,k1)::l1) ) ; cycle = c1}

type position =
  | Center
  | PLeft
  | PRight
  | PTop
  | PBot
  | UpLeft
  | UpRight
  | LowLeft
  | LowRight

type picture = 
  | Tex of string
(* later in the mlpost module *)
open Style

type command = 
  | Draw of t * style option
  | Label of picture * position * point
  | DotLabel of picture * position * point

type figure = command list

let label ?(pos=Center) pic point = Label (pic,pos,point)
(* replace later *)
let dotlabel ?(pos=Center) pic point = DotLabel (pic,pos,point)

let draw ?color t = 
  (* We don't use a default to avoid the output of 
     ... withcolor (0.00red+0.00green+0.00blue) withpen .... 
     for each command in the output file *)
  match color with
    | Some c -> Draw (t, Some (mk_style c))
    | None -> Draw (t, None)

let tex s = Tex s

let print_float fmt f =
  if f = infinity then F.fprintf fmt "infinity"
  else F.fprintf fmt "%g" f

let print_num fmt = function
  | BP f -> F.fprintf fmt "%a" print_float f 
  | PT f -> F.fprintf fmt "%apt" print_float f 
  | CM f -> F.fprintf fmt "%acm" print_float f 
  | MM f -> F.fprintf fmt "%amm" print_float f 
  | IN f -> F.fprintf fmt "%ain" print_float f 

let print_point fmt = function
  | Up -> F.fprintf fmt "up"
  | Down -> F.fprintf fmt "down"
  | Left -> F.fprintf fmt "left"
  | Right -> F.fprintf fmt "right"
  | Dir d -> F.fprintf fmt "dir %a" print_float d
  | Ppair (m,n) -> F.fprintf fmt "(%a,%a)" print_num m print_num n

let print_joint fmt = function
  | JLine -> F.fprintf fmt "--"
  | JCurve -> F.fprintf fmt ".."
  | JCurveNoInflex -> F.fprintf fmt "..."
  | JTension (a,b) -> F.fprintf fmt "..tension %a and %a .." print_float a print_float b
  | JControls (a,b) -> F.fprintf fmt "..controls %a and %a .." print_point a print_point b

let print_dir fmt = function
  | NoDir -> ()
  | Vec p -> F.fprintf fmt "{%a}" print_point p
  | Curl f -> F.fprintf fmt "{curl %a}" print_float f

let print_knot fmt (d1,p,d2) = 
  F.fprintf fmt "%a%a%a" print_dir d1 print_point p print_dir d2

let print_subpath fmt (k,l) =
  let rec print_jklist fmt = function
    | [] -> ()
    | (j,k) :: rl ->
        F.fprintf fmt "%a %a %a" print_jklist rl print_knot k print_joint j 
  in
    F.fprintf fmt "%a %a" print_jklist l print_knot k

let print_path fmt {path = p; cycle = c } =
  match c with
    | None -> print_subpath fmt p
    | Some j -> 
        F.fprintf fmt "%a %a cycle" print_subpath p print_joint j

let print_position fmt = function
  | Center  -> F.fprintf fmt ""
  | PLeft   -> F.fprintf fmt ".lft"
  | PRight  -> F.fprintf fmt ".rt"
  | PTop    -> F.fprintf fmt ".top"
  | PBot    -> F.fprintf fmt ".bot"
  | UpLeft  -> F.fprintf fmt ".ulft"
  | UpRight -> F.fprintf fmt ".urt"
  | LowLeft -> F.fprintf fmt ".llft"
  | LowRight -> F.fprintf fmt ".lrt"

let print_pic fmt = function
  | Tex s -> F.fprintf fmt "btex %s etex" s

let print_command fmt  = function
  | Draw (p, None) ->
      F.fprintf fmt "draw %a;@\n" print_path p
  | Draw (p, Some s) -> 
      F.fprintf fmt "draw %a %a;@\n" 
	print_path p print_style s
  | Label (pic,pos,p) ->
      F.fprintf fmt "label%a(%a,%a); @\n"
        print_position pos print_pic pic print_point p
  | DotLabel (pic,pos,p) ->
      F.fprintf fmt "dotlabel%a(%a,%a); @\n"
        print_position pos print_pic pic print_point p

let print_fig i fmt l =
  F.fprintf fmt "beginfig(%d)@\n %a endfig;@." i 
    (fun fmt l -> List.iter (print_command fmt) l)
    l
