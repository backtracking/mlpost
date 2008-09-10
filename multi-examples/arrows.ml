open Examples
open Mlpost
open Command

let w = 2.
let a = Point.pt (Num.cm 0., Num.cm 0.)
let b = Point.pt (Num.cm w, Num.cm 0.)
let c = Point.pt (Num.cm 0., Num.cm w)
let d = Point.pt (Num.cm w, Num.cm w)

let up = Path.Vec Point.up
let down = Path.Vec Point.down
let left = Path.Vec Point.left
let right = Path.Vec Point.right

let () =
  emit [Arrow.draw a d];
  emit [Arrow.draw b c];
  emit [Arrow.draw c b];
  emit [Arrow.draw a b];
  (* Some curved arrows *)
  emit [Arrow.draw ~outd: up a d];
  emit [Arrow.draw ~outd: up b c];
  emit [Arrow.draw ~outd: right c b];
  emit [Arrow.draw ~outd: up a b];
  (* Some double-headed arrows *)
  emit [Arrow.draw ~foot: Arrow.simple_head a d];
  emit [Arrow.draw ~foot: (Arrow.simple_head ~angle: 300.) b c];
  emit [Arrow.draw ~foot: Arrow.simple_head ~outd: right c b];
  emit [Arrow.draw ~foot: (Arrow.simple_head ~angle: 180.) ~outd: up a b];
  (* A snake arrow *)
  emit [Arrow.draw
            ~foot: (Arrow.simple_head ~angle: 300.)
            ~outd: up ~ind: up a b];
  (* A straight thick arrow *)
  emit [Arrow.draw_thick a d];
  emit [Arrow.draw_thick b c];
  emit [Arrow.draw_thick c b];
  emit [Arrow.draw_thick a b];
  (* Some curved thick arrows *)
  emit [Arrow.draw_thick ~outd: up a d];
  emit [Arrow.draw_thick ~outd: up b c];
  emit [Arrow.draw_thick ~outd: right c b];
  emit [Arrow.draw_thick ~outd: up a b];
  (* A snake thick arrow *)
  emit [Arrow.draw_thick ~outd: up ~ind: up a b];
  dump ()
