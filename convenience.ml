module P = Path
open Helpers

type numtype = BP | PT | CM | MM | IN

let get_joint = function
  | P.JLine -> straight
  | P.JCurve -> curved
  | _ -> assert false

let get_unit = function
  | BP -> P.bp
  | PT -> P.pt
  | IN -> P.inch
  | CM -> P.cm
  | MM -> P.mm

let path ?(style=P.JCurve) ?(cycle=false) ?(scale=BP) l =
  let p = get_joint style (num_lift (get_unit scale) l) in
    if cycle then P.cycle style p else p 

let draw ?(style=P.JCurve) ?(cycle=false) ?(scale=BP) l =
  P.draw (path ~style:style ~cycle:cycle ~scale:scale l)

let jointpath ?(scale=BP) lp lj =
  let s = get_unit scale in
    jointpath (List.map (fun (a,b) -> (P.NoDir, P.p (s a, s b), P.NoDir)) lp) lj

let p ?(l=P.NoDir) ?(r=P.NoDir) ?(scale=BP) (a,b) =
  let s = get_unit scale in
    (l, P.p (s a, s b), r)
