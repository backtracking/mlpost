      let rec f_on f1 = function
        | [] -> [f1]
        | On f2::l -> f_on (f1+.num f2) l
        | Off  f2::l -> f1::(f_off f2 l)
      and f_off f1 = function
        | [] -> [f1]
        | On f2::l -> f_on (f1+.num f2) l
        | Off  f2::l -> f1::(f_off f2 l)
      and f_first = function
        | [] -> []
        | On f2::l -> f_on f2 l
        | Off f2::l -> 0.::(f_off f2 l)
