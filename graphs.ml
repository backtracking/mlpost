open Mlpost

let map_bp = List.map (fun (a,b) -> (bp a, bp b))

let draw1 = 
  [ draw (straight (map_bp [20.,20.; 0.,0.; 0.,30.; 30.,0.; 0.,0.]))]

let draw2 =
  [ draw (curved (map_bp [0.,0.; 60.,40.; 40.,90.; 10., 70.;30.,50.]))]

let draw3 =
  [ draw 
      (cycle
         (curved 
          (map_bp [0.,0.; 60.,40.; 40.,90.; 10., 70.;30.,50.])))]

let _ = print_endline "hello world"
