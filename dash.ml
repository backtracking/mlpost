
open Format

type on_off = On of Num.t | Off of Num.t

type t =
  | Evenly
  | Withdots
  | Scaled of float * t
  | Shifted of Point.t * t
  | Pattern of on_off list

let evenly = Evenly
let withdots = Withdots
let scaled s d = Scaled (s, d)
let shifted s d = Shifted (s, d)
let pattern l = Pattern l

let rec print fmt = function
  | Evenly -> fprintf fmt "evenly"
  | Withdots -> fprintf fmt "withdots"
  | Scaled (s, d) -> fprintf fmt "%a scaled %a" print d Num.print_float s
  | Shifted (p, d) -> fprintf fmt "%a shifted %a" print d Point.print p
  | Pattern l -> 
      fprintf fmt "dashpattern(";
      List.iter 
	(fun p -> 
	  let p,n = match p with On n -> "on", n | Off n -> "off", n in
	  fprintf fmt "%s %a " p Num.print n) 
	l;
      fprintf fmt ")" 

