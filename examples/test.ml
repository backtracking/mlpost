open Num
open Command
open Point
open Path
open Helpers


let (++) x y = pt (cm x, cm y)

let fig =
  let a = Box.circle (Picture.tex "$\\sqrt2$") in
  let b = 
    Box.shift (2. ++ 0.) (Box.rect ~fill:Color.purple (Picture.tex "$\\pi$"))
  in
  let pen = Pen.default ~tr:[Transform.scaled (bp 3.)] () in
  [ Box.draw a;
    Box.draw b;
    draw ~color:Color.red (Path.shift (1. ++ 1.) (Box.bpath a));
    draw_label_arrow ~color:Color.orange ~pen 
    ~pos:`Upright (Picture.tex "foo") (Box.west a) (Box.south_east b);
    box_arrow ~color:Color.blue a b;
]
