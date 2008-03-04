open Path
open Point
open Num
open Command

(* generic functions that proved to be useful *)
(* might move into the interface to obtain better metapost *)

let ($$) f x = f x
(* map over pairs *)
let pmap f (a,b) = (f a, f b)

let point scale fpair = p (pmap scale fpair)

(* construct a point with the right measure *)
let bpp, inp, cmp, mmp, ptp = point bp, point inch, point cm, point mm, point pt

(* construct a list of points with the right measure *)
let map_bp, map_in, map_cm, map_mm, map_pt =
  List.map bpp,
  List.map inp,
  List.map cmp,
  List.map mmp,
  List.map ptp

(* construct the value f (f (... f(f acc st ) (st + 1) ...) (en -1)) en *)
let rec fold_from_to f st en acc =
  if st <= en then
    fold_from_to f (st+1) en (f acc st)
  else
    acc

(* iterate from [st] to [en] *)
let iter_from_to f st en =
  fold_from_to (fun () i -> f i) st en ()

(* map [st; ...; en] to [ f st; ...; f en] *)
let map_from_to f st en =
  fold_from_to (fun acc i -> (f i)::acc ) st en []

(* construct a path with a given style from a knot list *)
let path_fold style l =
  match l with
    | [] -> failwith "empty path is not allowed"
    | (x::xs) ->
        List.fold_left (fun p knot -> concat p style knot) (start x) xs

let point_fold style l =
  path_fold style
    (List.map (fun p -> (NoDir, p, NoDir)) l)

(* construct a straight path from a point list *)
let straight l = point_fold JLine
(* construct a curved path from a point list *)
let curved l = point_fold JCurve

(* construct a path with knot list and joint list *)
let jointpath lp lj =
  List.fold_left2 concat (start (List.hd lp))
    lj (List.tl lp)

(*  puts labels at given points with given text *)
let dotlabels ?(pos=Center) ls lp =
  List.map2 (fun s p -> dotlabel ~pos:pos (Picture.tex s) p) ls lp
