open Mlpost
open Num
open Command
open Point
open Path
open Helpers

let cmp x y = Point.cmp (x, y)

let fig = 
  let a = Box.circle (cmp 0. 0.) (Picture.tex "$\\sqrt2$") in
  let b = Box.rect (cmp 2. 0.) (Picture.tex "$\\pi$") in
  let pen = Pen.default ~tr:[Transform.scaled (f 3.)] () in
  [ draw_box a;
    draw_box ~fill:Color.purple b;
    draw
      ~color:Color.red
      (Path.shift (cmp 1. 1.) (Box.bpath a));
    draw_label_arrow ~color:Color.orange ~pen 
      ~pos:Pupright (Picture.tex "foo") (Box.west a) (Box.south_east b);
    box_arrow ~color:Color.blue a b;
  ]
