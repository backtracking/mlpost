open Mlpost
open Command
open Picture
open Point
open Path
module H = Helpers

let z0 = 0.,0.
let z1 = 60.,40.
let z2 = 40.,90.
let z3 = 10.,70.
let z4 = 30.,50.
let l1 = z0::z1::z2::z3::z4::[]

let labels1 =
   (H.dotlabels ~pos:Ptop ["0";"2";"4"] (map_bp [z0;z2;z4])) @
   [dotlabel ~pos:Pleft (tex "3") (bpp z3);
    dotlabel ~pos:Pright (tex "1") (bpp z1) ]

let fig = [ draw (path ~style:JCurve  l1) ] @ labels1

