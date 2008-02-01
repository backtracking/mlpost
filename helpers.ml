open Path

(* generic functions that proved to be useful *)
(* might move into the interface to obtain better metapost *)
let pair_apply f (a,b) = p (f a, f b)
let num_lift f l = List.map (pair_apply f) l
let bpp pair = pair_apply bp pair
let map_bp = num_lift bp
let inp pair = pair_apply inch pair

(* construct the value f (f (... f(f acc st ) (st + 1) ...) (en -1)) en *)
let rec fold_from_to f st en acc =
  if st <= en then
    fold_from_to f (st+1) en (f acc st)
  else
    acc

let iter_from_to f st en =
  fold_from_to (fun () i -> f i) st en ()

(* map [st; ...; en] to [ f st; ...; f en] *)
let map_from_to f st en =
  fold_from_to (fun acc i -> (f i)::acc ) st en []

let path_fold style l =
  match l with
    | [] -> failwith "empty path is not allowed"
    | (x::xs) ->
        List.fold_left (fun p knot -> concat p style knot) (start x) xs

let straight l =
  path_fold JLine
    (List.map (fun p -> (NoDir, p, NoDir)) l)

let curved l =
  path_fold JCurve
    (List.map (fun p -> (NoDir, p, NoDir)) l)

let jointpath lp lj =
  List.fold_left2 concat (start (List.hd lp))
    lj (List.tl lp)

let dotlabels ?(pos=Center) ls lp =
  List.map2 (fun s p -> dotlabel ~pos:pos (tex s) p) ls lp
