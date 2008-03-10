open Mlpost
open SimplePoint
open Command
open Dash
open Num
module SP = SimplePath

let fig = 
  [draw ~dashed:(pattern [On (bp 2.) ; Off (bp 3.)] ) 
     (SP.path ~scale:Num.cm [0.,0.; 3.,0.]) ; ]
