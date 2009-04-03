type t = Cairo.matrix

include Cairo.Matrix

let translation = init_translate
let rotation = init_rotate
let scale = init_rotate

let to_cairo x = x
let identity = init_identity

let transform = Cairo.transform
