open Point
open Command
open Dash
open Num
module SP = Path

let fig = 
  [draw ~dashed:(pattern [on (bp 1.) ; off (bp 2.); on (bp 10.); off (bp 2.)]) 
     (SP.path ~scale:Num.cm [0.,0.; 3.,0.]) ; ]
