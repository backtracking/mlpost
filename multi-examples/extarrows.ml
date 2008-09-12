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

let double_headed = ExtArrow.add_foot ExtArrow.simple

let multiple_headed =
  ExtArrow.add_belt ~point: 0.
    (ExtArrow.add_belt ~point: 0.25
       (ExtArrow.add_belt ~point: 0.5
          (ExtArrow.add_belt ~point: 0.75 ExtArrow.simple)))

let () =
  emit [ExtArrow.draw2 a d];
  emit [ExtArrow.draw2 b c];
  emit [ExtArrow.draw2 c b];
  emit [ExtArrow.draw2 a b];
  (* Some curved arrows *)
  emit [ExtArrow.draw2 ~outd: up a d];
  emit [ExtArrow.draw2 ~outd: up b c];
  emit [ExtArrow.draw2 ~outd: right c b];
  emit [ExtArrow.draw2 ~outd: up a b];
  (* Some double-headed arrows *)
  emit [ExtArrow.draw2 ~kind: double_headed a d];
  emit [ExtArrow.draw2 ~kind: double_headed ~outd: right c b];
  (* Some multiple-headed arrows *)
  emit [ExtArrow.draw2 ~kind: multiple_headed a d];
  emit [ExtArrow.draw2 ~kind: multiple_headed ~outd: right c b];
  dump ()
