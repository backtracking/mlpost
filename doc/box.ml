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
