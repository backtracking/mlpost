open Point
open Command
open Dash
open Num
module SP = Path

let fig = 
  [draw ~dashed:(pattern [on (bp 2.) ; off (bp 3.)] ) 
     (SP.path ~scale:Num.cm [0.,0.; 3.,0.]) ; ]
