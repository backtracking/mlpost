open Mlpost

let draw1 = 
  let map_bp = List.map (fun (a,b) -> (bp a, bp b)) in
    [ draw 
       (straight 
          (map_bp [20.,20.; 0.,0.; 0.,30.; 30.,0.; 0.,0.]))]

let _ = print_endline "hello world"
