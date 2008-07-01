open Mlpost
open Point
open Command
open Dash
open Num
module SP = Path

let fig = 
  [draw ~dashed:(pattern [On (bp 2.) ; Off (bp 3.)] ) 
     (SP.path ~scale:Num.cm [0.,0.; 3.,0.]) ; ]
