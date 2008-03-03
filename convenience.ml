module P = Path
open Helpers

let path ?(style=P.JCurve) ?(cycle) ?(scale=Num.bp) l  =
  let sc = List.map (fun pr -> Point.p (pmap scale pr)) in
  let p = point_fold style (sc l) in
    match cycle with
      | None -> p
      | Some cst -> P.cycle P.NoDir cst p

let p ?(l=P.NoDir) ?(r=P.NoDir) ?(scale=Num.bp) (a,b) =
  let s pair = Point.p (pmap scale pair) in
    (l, s (a, b), r)

let draw ?(style) ?(cycle) ?(scale) ?(color) ?(pen) l =
   Command.draw ?color ?pen (path ?style ?cycle ?scale l)

let jointpath ?(scale=Num.bp) lp lj  =
  let s pair =  Point.p (pmap scale pair) in
    jointpath (List.map (fun (a,b) -> (P.NoDir, s (a, b), P.NoDir)) lp) lj

