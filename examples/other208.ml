open Mlpost
open Path
open SimplePoint
open Command
module SP = SimplePath

type t = One | Two | Three | Four

let d0 = 1.
let d1 = sqrt (2.*.(1.+.cos(72.*.2.*.3.14159/.360.)))
let d2 = sqrt (2.*.(1.-.cos(36.*.2.*.3.14159/.360.)))
let r0 = d0 /. (d0 +. d2)
let r1 = d1 /. (d0 +. d1)

let rec pave t a b c n =
  if n > 0 then
    match t with 
      | One -> 
	  let d = Point.segment r0 a c in
	    (pave One b c d (n-1))@
	      (pave Four b d a (n-1))
      | Two ->
	  let d = Point.segment r0 a b in
	    (pave Two c d b (n-1))@
	      (pave Three c a d (n-1))	    
      | Three ->
	  let d = Point.segment r1 a b in
	  let e = Point.segment r0 a c in
	    (pave One d c e (n-1))@
	      (pave Three b c d (n-1))@pave Four d e a (n-1)
      | Four -> 
	  let d = Point.segment r1 a c in
	  let e = Point.segment r0 a b in
	    (pave Two d e b (n-1))@
	      (pave Three d a e (n-1))@pave Four c d b (n-1)
  else  
    let pen = Pen.transform [Transform.scaled 1.] Pen.circle in
    let gb = Color.rgb 0. 1. 1. in
    let gr = Color.rgb 1. 1. 0. in
    let path = SP.pathp ~style:JLine ~cycle:JLine [a;b;c] in
    let color, segs =
      match t with 
      | One -> gb, [a;c]::[a;b]::[]
      | Two -> gb, [a;b]::[a;b]::[]
      | Three -> gr, [a;c]::[c;b]::[]
      | Four -> gr, [b;c]::[a;b]::[]
    in
      [draw path; fill path ~color]@
	(List.map (fun l -> draw ~pen (SP.pathp ~style:JLine l)) segs)

let fig = 
  let a = cmp (0., 0.) in
  let b = cmp (3., 0.) in
  let d = Point.rotated 72. b in
  let c = Point.add d (cmp (3.,0.)) in
    (pave Three a c d 6)@(pave Four a b c 6)
