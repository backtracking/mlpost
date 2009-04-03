type t = Cairo.point = 
    {x : float;
     y : float}

type point = t

let add a b = {x = a.x+.b.x; y = a.y+.b.y}
let minus a b = {x = a.x-.b.x; y = a.y-.b.y}
let half a = {x = a.x/.2.; y = a.y/.2.}
let double a = {x = a.x*.2.; y = a.y*.2.}
let scal a c = {x = a.x*.c; y = a.y*.c}
let add_half a b = half (add a b)

let to_cairo_point x = x
