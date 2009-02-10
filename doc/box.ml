open Mlpost
open Command
open Box
open Num

let circle = draw (circle (empty ~height:(bp 5.) ~width:(bp 5.) ()))
let rect = draw (rect (empty ~height:(bp 5.) ~width:(bp 5.) ()))
let round_rect = draw (round_rect (empty ~height:(bp 5.) ~width:(bp 5.) ()))
let ellipse = draw (ellipse (empty ~width:(bp 5.) ()))
let patatoid = draw (patatoid (empty ~height:(bp 5.) ~width:(bp 10.) ()))
let tex = draw (tex "text")

let brect = Box.rect (empty ~height:(bp 5.) ~width:(bp 5.) ())
let dot p = Command.draw ~pen:(Pen.scale (bp 4.) Pen.circle) (Path.pathp [p])

let ctr = seq [ draw brect; dot (ctr brect) ]
let north = seq [ draw brect; dot (north brect) ]
let south = seq [ draw brect; dot (south brect) ]
let west = seq [ draw brect; dot (west brect) ]
let east = seq [ draw brect; dot (east brect) ]
let north_west = seq [ draw brect; dot (north_west brect) ]
let south_west = seq [ draw brect; dot (south_west brect) ]
let north_east = seq [ draw brect; dot (north_east brect) ]
let south_east = seq [ draw brect; dot (south_east brect) ]

open Arrow
let kind = add_foot (add_head (add_line empty))

let cpic c = Box.pic ~stroke:None (Picture.make c)

let dbl_arrow = 
  let ar = 
    Arrow.point_to_point ~kind Point.origin (Point.pt (bp 10.,Num.zero))
  in
  cpic ar
let width = Box.draw (Box.vbox [ brect; dbl_arrow; ])
let height = Box.draw (Box.hbox [ Box.rotate 90. dbl_arrow;  brect ])

let fnstex s = Picture.tex (Format.sprintf "{\\footnotesize %s}" s)

let shift = 
  let pt = Point.pt (bp 40., bp 25.) in
  let vec = 
    cpic (
      seq [Arrow.point_to_point Point.origin pt; 
          Command.dotlabel ~pos:`Top (fnstex "pt") pt;
          Command.dotlabel ~pos:`Bot (fnstex "(0,0)") Point.origin;
]) in
  let b = brect in
  let b' = Box.shift pt b in
  let shift = 
    cpic ( seq [Box.draw b; Box.draw b';
             Arrow.point_to_point (Box.ctr b) (Box.ctr b')])
  in
  Box.draw (Box.hbox [vec; shift])

let center = 
  let pt = Point.pt (bp 40., bp 25.) in
  let vec = 
      seq [Arrow.point_to_point Point.origin pt; 
          Command.dotlabel ~pos:`Top (fnstex "pt") pt; ] in
  let b = brect in
  let b' = Box.center pt b in
  seq [vec; Box.draw b; Box.draw b']




let _ = Metapost.emit "circle" circle
let _ = Metapost.emit "rect" rect
let _ = Metapost.emit "round_rect" round_rect
let _ = Metapost.emit "ellipse" ellipse
let _ = Metapost.emit "patatoid" patatoid
let _ = Metapost.emit "tex" tex
let _ = Metapost.emit "ctr" ctr
let _ = Metapost.emit "north" north
let _ = Metapost.emit "south" south
let _ = Metapost.emit "west" west  
let _ = Metapost.emit "east" east  
let _ = Metapost.emit "north_west" north_west
let _ = Metapost.emit "south_west" south_west
let _ = Metapost.emit "north_east" north_east
let _ = Metapost.emit "south_east" south_east
let _ = Metapost.emit "width" width
let _ = Metapost.emit "height" height
let _ = Metapost.emit "shift" shift
let _ = Metapost.emit "center" center
