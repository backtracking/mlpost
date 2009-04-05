
let nop = []
module Dash = 
struct
  type t = float * float list

  type input_dash = 
    | On of float
    | Off of float

  let shifted f (x,d) = (x+.f,d)

  let line = 0., [3.; 3. ]
  let dots = 0., [0.; 5.]

  let rec on acc = function
    | [] -> [acc]
    | On f::l -> on (f +. acc) l
    | Off f::l -> acc::(off f l)
  and off acc = function
    | [] -> [acc]
    | On f::l -> acc::(on f l)
    | Off f::l -> off (f +. acc) l
  and to_dash = function
    | [] -> []
    | On f::l -> on f l
    | Off f::l -> 0. :: (off f l)

  let pattern l = 
    0., to_dash l

  let scale f (x,l) = 
    x, List.map (fun z -> f *. z) l
end
