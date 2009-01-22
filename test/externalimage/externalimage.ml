open Mlpost

let debug = false
let filename = "powered-by-caml.png"

let height = Num.cm 2.
let width = Num.cm 1.

let square = 
  let rect = Shapes.rectangle width height in
  Command.draw (Path.shift (Point.scale (Num.bp 0.5) (Point.pt (width,height))) rect)
let extim x = if debug then Command.nop else (Command.externalimage filename x)

let image1 = Command.seq [square;extim (`Exact (height,width))]
let image2 = Command.seq [square;extim (`Inside (height,width))]
let image3 = Command.seq [square;extim (`Height height)]
let image4 = Command.seq [square;extim (`Width width)]


let _ = 
  List.iter (fun (name,fig) -> Metapost.emit name fig;Metapost.emit (name^"_ro") (Command.draw_pic (Picture.rotate 128. (Picture.make fig))))
  [ "image1", image1;
    "image2", image2;
    "image3", image3;
    "image4", image4;
  ]

