open Mlpost
open Path
open Num
open Point

let fig =
  let () = Random.self_init () in
  let branchrotation = 60. in
  let offset = 180. -. branchrotation  in
  let thinning =  0.7 in
  let shortening = 0.8 in
  let drawit a b thickness =
    let pen = Pen.circle ~tr:[Transform.scaled (bp thickness)] () in
    Command.draw ~pen (pathp [a ; b])
  in
  let randrotate a b neg =
    let r = offset +. Random.float branchrotation in
    let r = if neg then (0. -. r) else r in
    let tr = [ Transform.rotate_around b r] in
      Point.transform tr a
  in
  let rec tree a b n size acc =
    let acc = (drawit a b size) :: acc in
    if n <= 0 then acc else
      let c = segment shortening b (randrotate a b false) in
      let d = segment shortening b (randrotate a b true) in
      let newsize = thinning *. size in
      let acc = tree b c (n-1) newsize acc in
          tree b d (n-1) newsize acc
  in
    tree (p (0.,0.)) (pt (bp 0., Num.cm 1.)) 10  2. []

