let pmap f (a,b) = (f a, f b)

let point ?(scale=Num.bp) pr =
  Point.p (pmap scale pr)

let ptlist ?scale l = List.map (point ?scale) l

(* construct a point with the right measure *)
let bpp, inp, cmp, mmp, ptp = 
    point ~scale:Num.bp, 
    point ~scale:Num.inch, 
    point ~scale:Num.cm, 
    point ~scale:Num.mm, 
    point ~scale:Num.pt

(* construct a list of points with the right measure *)
let map_bp, map_in, map_cm, map_mm, map_pt =
  ptlist ~scale:Num.bp, 
  ptlist ~scale:Num.inch, 
  ptlist ~scale:Num.cm, 
  ptlist ~scale:Num.mm, 
  ptlist ~scale:Num.pt
