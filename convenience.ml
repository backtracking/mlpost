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

let draw ?(style=P.JCurve) ?(cycle=false) ?(scale=BP) l =
  let p = get_joint style (num_lift (get_unit scale) l) in
  let p = if cycle then P.cycle style p else p in
    P.draw p
