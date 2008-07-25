type 'a pos = 
  { v : 'a; size : Num.t * Num.t; 
    center : Point.t ;
    move: Point.t -> 'a -> 'a }

let height posobj = snd (posobj.size)
let width posobj = fst (posobj.size)

open Num.Infix

let posmove pl p l = List.map2 (fun pos x -> pos.move p x) pl l

let halign ?(dx=Num.zero) pl =
  let hmax = Num.fold_max height Num.zero pl in
  let hmax_2 = hmax // Num.two in
  let rec make_new acc x = function
    | [] -> List.rev acc, x
    | p :: pl ->
	let wp,hp = p.size in
	let c = Point.pt (x +/ dx +/ wp // Num.two, hmax_2) in 
        let b = p.move (Point.sub c p.center) p.v in
	  make_new (b::acc) (x +/ wp +/ dx +/ dx) pl
  in
  let l,x = make_new [] Num.zero pl in
  let mycenter = Point.pt (x // Num.two, hmax_2) in
  { v = l; size = (x, hmax);
    (* center the whole haligned object at c *)
    center = mycenter;
    move = posmove pl}

let valign ?(dy=Num.zero) pl =
  let wmax = Num.fold_max width Num.zero pl in
  let wmax_2 = wmax // Num.two in
  let rec make_new acc y = function
    | [] -> List.rev acc, y
    | p :: pl ->
	let wp,hp = p.size in
	let c = Point.pt (wmax_2, y -/ dy -/ hp // Num.two) in 
        let b = p.move (Point.sub c p.center) p.v in
	  make_new (b::acc) (y -/ hp -/ dy -/ dy) pl
  in
  let l,y = make_new [] Num.zero pl in
  let mycenter = Point.pt (wmax_2, y // Num.two) in
  { v = l; size = (wmax,y);
    (* center the whole haligned object at c *)
    center =  mycenter;
    move = posmove pl}

let from_pic p = 
  { v= p; size = (Picture.width p, Picture.height p);
    center = Picture.ctr p;
    move = Picture.shift }

let to_pic p = p.v

