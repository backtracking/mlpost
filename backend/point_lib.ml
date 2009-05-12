type t = Cairo.point = 
    {x : float;
     y : float}

type point = t

let add a b = {x = a.x+.b.x; y = a.y+.b.y}
let sub a b = {x = a.x-.b.x; y = a.y-.b.y}
let mult c a = {x = a.x*.c; y = a.y*.c}
let div a c = {x = a.x/.c; y = a.y/.c}

let transform t p = Cairo.Matrix.transform_point t p
let rotated f = transform (Cairo.Matrix.init_rotate f)

let swapmx {x=x;y=y} = {x=y;y= -.x}
let swapmy {x=x;y=y} = {x= -.y;y=x}

let to_cairo_point x = x

module Infix = 
struct
  let (+/)  = add
  let (-/)  = sub
  let ( */)  = mult
  let ( //) = div
end
open Infix

let segment f p1 p2 = (1.-.f) */ p1 +/ f */ p2
let middle = segment 0.5
let print fmt x = Format.fprintf fmt "(%f,%f)" x.x x.y

let norm2 p : float = p.x*.p.x+.p.y*.p.y
let norm p = sqrt (norm2 p)

let zero = {x=0.;y=0.}
