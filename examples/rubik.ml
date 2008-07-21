open Mlpost
open Command
open Picture
open Path
open Num
open Num.Infix

let alpha = atan 1.
let beta = atan 1. /. 2. 
let mag = 10.

let proj x y z = 
  mag *. float (x - y) *. cos alpha, 
  mag *. (float (x + y) *. sin alpha *. sin beta +. float z *. cos beta)

let pen = Pen.default ~tr:([Transform.scaled (f 2.5)]) ()

let square color p i j =
  let pt i j = let x,y = p i j in Point.pt (bp x, bp y) in
  let points = [pt i j; pt (i+1) j; pt (i+1) (j+1); pt i (j+1)] in
  let path = pathp ~style:JLine ~cycle:JLine points in
  seq [fill ~color path; draw ~pen path]

let right = square Color.orange (fun i j -> proj i 0 j)
let up = square Color.yellow (fun i j -> proj i j 3)
let left = square Color.green (fun i j -> proj 0 (3 - i) j)

let fig = 
  [iter 0 2 (fun i -> iter 0 2 (right i));
   iter 0 2 (fun i -> iter 0 2 (up i));
   iter 0 2 (fun i -> iter 0 2 (left i));]
