open Mlpost
open SimplePoint
open Command
open Dash
open Num
module SP = SimplePath

let fig = 
  [draw ~dashed:(pattern [On (bp 1.) ; Off (bp 2.); On (bp 10.); Off (bp 2.)]) 
     (SP.path ~scale:Num.cm [0.,0.; 3.,0.]) ; ]