type command = Spline_lib.path list

let nop = []
module Dash = 
struct
  type dash = 
    | On of float
    | Off of float

  let rec on acc = function
    | [] -> [acc]
    | On f::l -> on (f +. acc) l
    | Off f::l -> acc::(off f l)
  and off acc = function
    | [] -> [acc]
    | On f::l -> acc::(on f l)
    | Off f::l -> off (f +. acc) l
  and f_first = function
    | [] -> []
    | On f::l -> on f l
    | Off f::l -> 0. :: (off f l)
end
