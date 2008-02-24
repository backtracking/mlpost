module P = Path
open Helpers

type numtype = BP | PT | CM | MM | IN

let get_joint = function
  | P.JLine -> straight
  | P.JCurve -> curved
  | _ -> assert false

let get_unit = function
  | BP -> bpp
  | PT -> ptp
  | IN -> inp
  | CM -> cmp
  | MM -> mmp

let get_lift = function
  | BP -> map_bp
  | PT -> map_pt
  | IN -> map_in
  | CM -> map_cm
  | MM -> map_mm

let path ?(style=P.JCurve) ?(cycle) ?(scale=BP) l  =
  let p = get_joint style (get_lift scale l) in
    match cycle with
      | None -> p
      | Some cst -> P.cycle P.NoDir cst p

let p ?(l=P.NoDir) ?(r=P.NoDir) ?(scale=BP) (a,b) =
  let s = get_unit scale in
    (l, s (a, b), r)

let draw ?(style) ?(cycle) ?(scale) ?(color) ?(pen) l =
   Mlpost.draw ?color ?pen (path ?style ?cycle ?scale l)

let jointpath ?(scale=BP) lp lj  =
  let s = get_unit scale in
    jointpath (List.map (fun (a,b) -> (P.NoDir, s (a, b), P.NoDir)) lp) lj

