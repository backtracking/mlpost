type t = Cairo.point = 
    {x : float;
     y : float}

type point = t

let pi = 3.1415926535897932384626433832795029
let pi_div_180 = pi /. 180.0
let deg2rad f = pi_div_180 *. f 

let add a b = {x = a.x+.b.x; y = a.y+.b.y}
let sub a b = {x = a.x-.b.x; y = a.y-.b.y}
let half a = {x = a.x/.2.; y = a.y/.2.}
let double a = {x = a.x*.2.; y = a.y*.2.}
let mult c a = {x = a.x*.c; y = a.y*.c}
let rotated a { x = x; y = y } = 
  let angle = deg2rad a in
  { x = cos angle *. x -. sin angle *. y;
    y = sin angle *. x +. cos angle *. y;
  }

let transform t p = Cairo.Matrix.transform_point t p
      
let add_half a b = half (add a b)

let to_cairo_point x = x

module Infix = 
struct
  let (+/)  = add
  let (-/)  = sub
  let ( */)  = mult
end
