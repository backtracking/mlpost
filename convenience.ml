module P = Path

type numtype = BP | PT | CM | MM | IN

let num_lift f p = List.map (fun (a,b) ->  f a, f b) p

let get_style = function
  | P.Straight -> P.straight
  | P.Curved -> P.curved

let get_unit = function
  | BP -> P.bp
  | PT -> P.pt
  | IN -> P.inch
  | CM -> P.cm
  | MM -> P.mm

let draw ?(style=P.Curved) ?(cycle=false) ?(scale=BP) l =
  let p = get_style style (num_lift (get_unit scale) l) in
  let p = if cycle then P.cycle style p else p in
    P.draw p
