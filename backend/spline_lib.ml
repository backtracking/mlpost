exception Not_implemented of string

let not_implemented s = raise (Not_implemented s)


type point = Point.point

let id x = x
open Point
open Point.Infix

let rec one_to_one f a = function
  | [] -> a
  | e::l -> one_to_one f (List.fold_left (f e) a l) l
          
let rec one_to_one2 f acc a b =
  List.fold_left (fun acc ea -> List.fold_left (fun acc eb -> f acc ea eb) acc b) acc a


let inter_depth = ref 15
let debug = ref false

type abscissa = float

type spline = {sa : point;
               sb : point;
               sc : point;
               sd : point;
               smin : abscissa;
               smax : abscissa;
               start : bool}

type path = {pl : spline list;
             cycle : bool}


let pt_f fmt p =
  Format.fprintf fmt "{@[ %.20g,@ %.20g @]}" p.x p.y

let print_spline fmt pt =
  Format.fprintf fmt "@[{ %a,@ %a,@ %a,@ %a }@]@." 
    pt_f pt.sa pt_f pt.sb pt_f pt.sc pt_f pt.sd

let printf fmt p = Format.fprintf fmt "@[[%a]" 
  (fun fmt -> List.iter (fun e -> Format.fprintf fmt "%a;" print_spline e)) p.pl


let s_of_01 s t = t*.(s.smax-.s.smin)+.s.smin
let _01_of_s s t = (t-.s.smin)/.(s.smax-.s.smin)

let create a b c d = {pl=[{sa = a;sb = b;sc = c; sd = d;smin = 0.;smax = 1.;start = true}];cycle=false}
let create_line a d = {pl=[{sa=a;sb=a;sc=d;sd=d;smin=0.;smax=1.;start=true}];cycle=false}
let create_lines l = 
  let rec aux = function
  |[] |[_] -> invalid_arg "The list must have at least 2 elements"
  |a::(d::_ as l) -> {sa=a;sb=a;sc=d;sd=d;smin=0.;smax=1.;start=false}::aux l in
  match aux l with
    |[] -> assert false
    |a::l -> {pl={a with start = true}::l;cycle=false}


let min_abscissa p = (List.hd p.pl).smin
let max_abscissa p =
  let rec aux = function
    | [] -> assert false
    | [a] -> a.smax
    | a::l -> aux l in
  aux p.pl


let add_end p c d =
  let rec aux p = 
    match p with
      | [] -> assert false (* a path always have one element *)
      | [{sc=mb;sd=a;smax=smax} as e] -> [e;{sa = a;sb = 2. */ a -/ mb;
                                             sc = c;sd = d;smin=smax;smax=smax+.1.; start = false}]
      | a::l -> a::(aux l) in
  {p with pl=aux p.pl}

let add_end_line p d =
  let rec aux p = 
    match p with
      | [] -> assert false (* a path always have one element *)
      | [{sc=mb;sd=a;smax=smax} as e] -> [e;{sa = a;sb = a;
                                             sc = d;sd = d;smin=smax;smax=smax+.1.; start = false}]
      | a::l -> a::(aux l) in
  {p with pl=aux p.pl}

let apply f s = f s.sa s.sb s.sc s.sd

let f4 f a b c d = f (f a b) (f c d)

let cubic a b c d t =
  t*.(t*.(t*.(d +. 3.*.(b-.c) -. a)+.3.*.(c -. (2.*.b) +. a))+.3.*.(b -. a))+.a
    (*  ((t^3)*(d - (3*c) + (3*b) - a)) + (3*(t^2)*(c - (2*b) + a)) + (3*t*(b - a)) + a*)
    (*  d*.(t**3.)+.3.*.c*.(t**2.)*.(1.-.t)+.3.*.b*.(t**1.)*.(1.-.t)**2.+.a*.(1.-.t)**3.*)

let cubic_point s t =
  {x=cubic s.sa.x s.sb.x s.sc.x s.sd.x t;
   y=cubic s.sa.y s.sb.y s.sc.y s.sd.y t;}

let cubic_point_s s t = cubic_point s (_01_of_s s t)

let abscissa_to_point p t = 
  let rec aux = function
    |[] -> invalid_arg "abscissa_to_point : the abscissa given is greater than max_abscissa" 
    | a::l when a.smax >= t -> cubic_point_s a t
    | _::l -> aux l in
  if min_abscissa p > t then 
    invalid_arg "abscissa_to_point : the abscissa given is smaller than min_abscissa"
  else aux p.pl

let direction_of_abscissa p t = 
  (* TODO *) assert false

let extremum conv l a b c d =
  let test s l = if s>=0. && s<=1. then (conv s)::l else l in
  let sol delta = (delta -. (2.*.b) +. a +. c)/.(a -. d +. (3.*.(c -. b))) in
  let delta = ((b*.b) -. (c*.(b +. a -. c)) +. (d*.(a -. b)))**0.5 in
  match compare delta 0. with
    | x when x<0 -> []
    | 0 -> test (sol 0.) l
    | _ -> test delta (test (-.delta) l)

let remarquable conv l a b c d = 0.::1.::(extremum conv l a b c d)

(*let extremum_point p = List.fold_left (fun s -> extremum s_of_01) p

let remarquable_point p = List.fold_left (fun l s -> s.smin::(s.smax::(extremum s_of_01 l s))) p
*)
(** simple intersection *)
let give_bound s =
  let x_max = f4 max s.sa.x s.sb.x s.sc.x s.sd.x in
  let y_max = f4 max s.sa.y s.sb.y s.sc.y s.sd.y in
  let x_min = f4 min s.sa.x s.sb.x s.sc.x s.sd.x in
  let y_min = f4 min s.sa.y s.sb.y s.sc.y s.sd.y in
  (x_min,y_min,x_max,y_max)
    
let list_min_max f p = 
 List.fold_left (fun (x_min,y_min,x_max,y_max) s ->
                                                    let (sx_min,sy_min,sx_max,sy_max) = f s in
                                                    (min x_min sx_min,min y_min sy_min,
                                                     max x_max sx_max,max x_max sy_max)) (infinity,infinity,neg_infinity,neg_infinity) p

let unprecise_bounding_box s = 
    let (x_min,y_min,x_max,y_max) = 
      list_min_max give_bound s.pl in
    ({x=x_min;y=y_min},{x=x_max;y=y_max})

let give_bound_precise s =
  let x_remarq = List.map (cubic s.sa.x s.sb.x s.sc.x s.sd.x) (remarquable id [] s.sa.x s.sb.x s.sc.x s.sd.x) in
  let y_remarq = List.map (cubic s.sa.y s.sb.y s.sc.y s.sd.y) (remarquable id [] s.sa.y s.sb.y s.sc.y s.sd.y) in
  let x_max = List.fold_left max neg_infinity x_remarq in
  let y_max = List.fold_left max neg_infinity y_remarq in
  let x_min = List.fold_left min infinity x_remarq in
  let y_min = List.fold_left min infinity y_remarq in
  (x_min,y_min,x_max,y_max)
    
let bounding_box s =   
  let (x_min,y_min,x_max,y_max) = 
    list_min_max give_bound_precise s.pl in
  ({x=x_min;y=y_min},{x=x_max;y=y_max})

let test_in amin amax bmin bmax =
  (amin <= bmax && bmin <= amax)
    
let is_intersect a b =
  let (ax_min,ay_min,ax_max,ay_max) = give_bound a in
  let (bx_min,by_min,bx_max,by_max) = give_bound b in
  test_in ax_min ax_max bx_min bx_max &&
    test_in ay_min ay_max by_min by_max
    
let is_intersect_precise a b =
  let (ax_min,ay_min,ax_max,ay_max) = give_bound_precise a in
  let (bx_min,by_min,bx_max,by_max) = give_bound_precise b in
  test_in ax_min ax_max bx_min bx_max &&
    test_in ay_min ay_max by_min by_max


let bisect a =
  let b = a in
  (*D\leftarrow (C+D)/2*)
  let b = {b with sd = middle b.sd b.sc} in
  (*C\leftarrow (B+C)/2, D\leftarrow (C+D)/2*)
  let b = {b with sc = middle b.sc b.sb} in
  let b = {b with sd = middle b.sd b.sc} in
  (*B\leftarrow (A+B)/2, C\leftarrow (B+C)/2, D\leftarrow(C+D)/2*)
  let b = {b with sb = middle b.sb b.sa} in
  let b = {b with sc = middle b.sc b.sb} in
  let b = {b with sd = middle b.sd b.sc} in
  let c = a in 
  let c = {c with sa = middle c.sa c.sb} in
  let c = {c with sb = middle c.sb c.sc} in
  let c = {c with sa = middle c.sa c.sb} in
  let c = {c with sc = middle c.sc c.sd} in
  let c = {c with sb = middle c.sb c.sc} in
  let c = {c with sa = middle c.sa c.sb} in
  (b,c)

type graph = {gt1 : int;
              gt2 : int;
              mutable gedge : graph list;
              mutable taken : bool;
              mutable gseenx : bool;
              mutable gseeny : bool}

exception Found of float*float

let one_intersection_aux () a b =
  if a=b then () else
    let nmax = 2.**(float_of_int (!inter_depth+1)) in
    let f_from_i s x = s_of_01 s ((float_of_int x)*.(1./.nmax)) in
    let rec aux a b t1 t2 dt= function
      | 0 -> 
          if is_intersect a b 
          then raise (Found (f_from_i a (t1+(dt/2)), f_from_i b (t2+(dt/2)))) 
          else ()
      | n -> let n = n-1 in
	let dt = dt/2 in
        if is_intersect a b then
          let (af,al) = bisect a in
          let (bf,bl) = bisect b in
          (* As Knuth said that doesn't find the first in the lexicographic order see §550 of metafont comment *)
          aux af bf t1 t2 dt n;
          aux af bl t1 (t2+dt) dt n;
          aux al bf (t1+dt) t2 dt n;
          aux al bl (t1+dt) (t2+dt) dt n in
    try 
      aux a b 0 0 (int_of_float nmax) !inter_depth
    with
        Found _ as res -> raise res

let one_intersection a b = 
  try
    one_to_one2 one_intersection_aux () a.pl b.pl;
    raise Not_found
  with
      Found (t1,t2) -> (t1,t2)

let intersection_aux acc a b =
  if a=b then [] else
    let rec aux a b l t1 t2 dt= function
      | 0 -> if is_intersect a b then (t1+(dt/2),t2+(dt/2))::l else l
      | n -> 
          if !debug then
            (Format.printf "%a" print_spline a;
	     Format.printf "%a" print_spline b);
          if is_intersect a b then
            let n = n-1 in
	    let dt = dt/2 in
            let (af,al) = bisect a in
            let (bf,bl) = bisect b in
            let l = aux af bf l t1 t2 dt n in
            let l = aux af bl l t1 (t2+dt) dt n in
            let l = aux al bf l (t1+dt) t2 dt n in
            let l = aux al bl l (t1+dt) (t2+dt) dt n in
            l
          else l in
    let rem_noise delta mdelta = function | [] -> [] | noisy ->
      let graph = List.map (fun (t1,t2) -> {gt1= t1;gt2=t2;gedge = [];taken = false;gseenx=false;gseeny=false}) noisy in
      let link sel msel seen = 
        let sorted = List.fast_sort (fun x y -> compare (sel x) (sel y)) graph in
        let rec pass bef = function
          |[] -> ()
          |e::l -> match ((sel e) - (sel bef) <= delta),abs ((msel e) - (msel bef)) <=mdelta, seen e with
              | false, _, false -> pass e l
              | false, _, true -> ()
              | true, true, false -> bef.gedge<-e::bef.gedge;
                  e.gedge<-bef::e.gedge;
                  pass e l
              | true, false, false -> pass e l; pass bef l
              | true, false, true -> pass bef l
              | true, true, true -> bef.gedge<-e::bef.gedge;
                  e.gedge<-bef::e.gedge in
        pass (List.hd sorted) (List.tl sorted) in
      link (fun x -> x.gt1)  (fun x -> x.gt2) (fun x -> x.gseenx);
      link (fun x -> x.gt2) (fun x -> x.gt1) (fun x -> x.gseeny);
      let rec connexe (((t1,t2),n) as t12n) e = 
        if e.taken then t12n
        else (e.taken <- true;
              let np = n+.1. in
              let t12n = ((t1*.(n/.np)+.(float_of_int e.gt1)/.np,
                           t2*.(n/.np)+.(float_of_int e.gt2)/.np),np) in
              List.fold_left connexe t12n e.gedge) in
      List.fold_left (fun l e -> if e.taken then l 
                      else (fst (connexe ((0.,0.),0.) e))::l) [] graph in
    let nmax = 2.**(float_of_int (!inter_depth+1)) in
    let l = aux a b [] 0 0 (int_of_float nmax) !inter_depth in
    (*if !debug then(
      Format.printf "@[";
      List.iter (fun (x,y) -> Format.printf "@[(%i,%i)@]" x y) l;
      Format.printf "@]@.");*)
    let delta = 2* !inter_depth in
    let mdelta = 16* !inter_depth in
    let l = rem_noise delta mdelta l in
    let f_from_i s x = s_of_01 s (x*.(1./.nmax)) in
    let res = List.fold_left (fun acc (x,y) -> (f_from_i a x,f_from_i b y)::acc) acc l in
    if !debug then
      Format.printf "@[%a@]@." (fun fmt -> List.iter (pt_f fmt))
        (List.map (fun (t1,t2) -> (cubic_point a t1) -/ (cubic_point b t2)) res);
    res


let intersection a b = one_to_one2 intersection_aux [] a.pl b.pl

let fold_left f acc p = List.fold_left (fun acc s -> f acc s.sa s.sb s.sc s.sd) acc p.pl
  
let iter f p =  List.iter (fun s -> f s.sa s.sb s.sc s.sd) p.pl

let union_conv ap bp = 
  let max = max_abscissa ap in
  let min = min_abscissa bp in
  let diff = max-.min in
  (fun x -> x +. diff)

let union ap bp = 
  let conv = union_conv ap bp in
  {pl=List.map (function {smin=smin;smax=smax} as b -> 
              {b with smin=conv smin;smax=conv smax}) bp.pl;cycle=false}

let reverse p =
  let conv = 
    let max = max_abscissa p in
    let min = min_abscissa p in
    let sum = max +. min in
    (fun x -> sum -. x) in
  let rec aux acc = function
    | [] -> acc
    | ({sa=sa;sd=sd;smin=smin;smax=smax} as a)::l -> 
        aux ({a with sa=sd;sd=sa;
                smin=conv smin; smax=conv smax}::acc) l in
  {p with pl = aux [] p.pl}

(*left ((t^3)*(d + (3*(b - c)) - a)) + ((t^2)*(d - (3*b) + (2*a))) + (t*((2*c) - b - a)) + b *)
(*right 3*d - c *)
let split_aux s t l = 
  if t = s.smax then ([s],l)
  else if t = s.smin then ([],s::l)
  else 
    let t0 = _01_of_s s t in
    let _1t0 = 1.-.t0 in
    let b1 = t0 */ s.sb +/ _1t0 */ s.sa in
    let c1 = 
      (t0*.t0) */ s.sc +/ (2.*.t0*._1t0) */ s.sb +/ (_1t0*._1t0) */ s.sa in
    let d1 = cubic_point s t0 in
    let a2 = d1 in
    let c2 = _1t0 */ s.sc +/ t0 */ s.sd in
    let b2 = 
      (_1t0*._1t0) */ s.sb +/ (2.*._1t0*.t0) */ s.sc +/ (t0*.t0) */ s.sd in
    ([{s with sb = b1;sd = d1;sc = c1;smax = t}],
     {s with sa = a2;sb = b2;sc = c2;smin = t}::l)

let split p t = 
  let rec aux = function
    |[] -> invalid_arg "abscissa_to_point : the abscissa given is greater than max_abscissa" 
    | a::l when a.smax > t -> split_aux a t l
    | a::l -> let (p1,p2) = aux l in (a::p1,p2) in
  if min_abscissa p >= t then 
    invalid_arg "abscissa_to_point : the abscissa given is smaller than min_abscissa"
  else 
    let (p1,p2) = aux p.pl in
    ({pl=p1;cycle = false},{pl=p2;cycle=false})

let subpath p t1 t2 = fst (split (snd (split p t1)) t2)

let cut_before a b = fst (split a (fst (one_intersection a b)))
let cut_after a b = reverse (fst (split a (fst (one_intersection (reverse a) b))))

let dicho_split x = assert false

let norm2 a b = a*.a +. b*.b
    
let dist_min_point_aux {x=px;y=py} pmin s =
  let is_possible_at a = 
    let (xmin,ymin,xmax,ymax) = give_bound a in
    match xmin > px, ymin > py, xmax < px, ymax < py with
      | true , true , _    , _     -> norm2 (xmin -. px) (ymin -. py)
      | _    , _    , true , true  -> norm2 (xmax -. px) (ymax -. py)
      | true , _    , _    , true  -> norm2 (xmin -. px) (ymax -. py)
      | _    , true , true , _     -> norm2 (xmax -. px) (ymin -. py)
      | false, true , false, _     -> norm2 0. (ymin -. py)
      | false, _    , false, true  -> norm2 0. (ymax -. py)
      | true , false, _    , false -> norm2 (xmin -. px) 0.
      | _    , false, true , false -> norm2 (xmax -. px) 0.
      | false, false, false, false -> 0. in
  let nmax = 2.**(float_of_int (!inter_depth+1)) in
  let f_from_i x = (float_of_int x)*.(1./.nmax) in
  let rec aux a ((min,_) as pmin) t1 dt = function
    | 0 -> let t1 = f_from_i (t1+(dt/2)) in
      let {x=apx;y=apy} = cubic_point s t1 in
      let dist = norm2 (apx -. px) (apy -. py) in
      if dist < min then (dist,s_of_01 s t1) else pmin
    | n -> let n = n-1 in
      let dt = dt/2 in
      let (af,al) = bisect a in
      let dist_af = is_possible_at af in
      let dist_al = is_possible_at al in
      let doit ((min,_) as pmin) dist am t = if dist < min then aux am pmin t dt n else pmin in
      if dist_af<dist_al then
        let pmin = doit pmin dist_af af t1 in
        doit pmin dist_al al (t1+dt)
      else
        let pmin = doit pmin dist_al al (t1+dt) in
        doit pmin dist_af af t1 in
  aux s pmin 0 (int_of_float nmax) !inter_depth
           
let dist_min_point p point = snd (List.fold_left (dist_min_point_aux point)
  (let one = (List.hd p.pl) in (norm2 (one.sa.x -. point.x) (one.sa.y -. point.y),one.smin)) p.pl)

let dist_min_path_aux pmin s1 s2 =
  let is_possible_at a b = 
    let (axmin,aymin,axmax,aymax) = give_bound a in
    let (bxmin,bymin,bxmax,bymax) = give_bound b in
    match axmin > bxmax, aymin > bymax, axmax < bxmin, aymax < bymin with
      | true , true , _    , _     -> norm2 (axmin -. bxmax) (aymin -. bymax)
      | _    , _    , true , true  -> norm2 (axmax -. bxmin) (aymax -. bymin)
      | true , _    , _    , true  -> norm2 (axmin -. bxmax) (aymax -. bymin)
      | _    , true , true , _     -> norm2 (axmax -. bxmin) (aymin -. bymax)
      | false, true , false, _     -> norm2 0. (aymin -. bymax)
      | false, _    , false, true  -> norm2 0. (aymax -. bymin)
      | true , false, _    , false -> norm2 (axmin -. bxmax) 0.
      | _    , false, true , false -> norm2 (axmax -. bxmin) 0.
      | false, false, false, false -> 0. in
  let nmax = 2.**(float_of_int (!inter_depth+1)) in
  let f_from_i x = (float_of_int x)*.(1./.nmax) in
  let rec aux a b ((min,_) as pmin) t1 t2 dt = function
    | 0 -> let t1 = f_from_i (t1+(dt/2)) in
      let t2 = f_from_i (t2+(dt/2)) in
      let ap = cubic_point s1 t1 in
      let bp = cubic_point s2 t2 in
      let dist = norm2 (ap.x -. bp.x) (ap.y -. bp.y) in
      if dist < min then (dist,(s_of_01 s1 t1,s_of_01 s2 t2)) else pmin
    | n -> let n = n-1 in
      let dt = dt/2 in
      let (af,al) = bisect a in
      let (bf,bl) = bisect b in
      let doit dist am bm t1 t2 ((min,_) as pmin) = if dist < min then aux am bm pmin t1 t2 dt n else pmin in
      let l = [af,bf,t1,t2;
               af,bl,t1,t2+dt;
               al,bf,t1+dt,t2;
               al,bl,t1+dt,t2+dt] in
      let l = List.map (fun (am,bm,t1,t2) -> let dist = is_possible_at am bm in
                        dist, doit dist am bm t1 t2) l in
      let l = List.fast_sort (fun (da,_) (db,_) -> compare da db) l in
      List.fold_left (fun pmin (_,doit) -> doit pmin) pmin l in
  aux s1 s2 pmin 0 0 (int_of_float nmax) !inter_depth

let dist_min_path p1 p2 = snd (one_to_one2 dist_min_path_aux
(let one1 = (List.hd p1.pl) in 
 let one2 = (List.hd p2.pl) in 
 (norm2 (one1.sa.x -. one2.sa.x) (one1.sa.y -. one2.sa.y),(one1.smin,one2.smin))) p1.pl p2.pl)

let translate p t = 
  {p with pl=List.map (function a ->
                         { a with sa= a.sa +/ t;
                             sb=a.sb +/ t;
                             sc=a.sc +/ t;
                             sd=a.sd +/ t}) p.pl}


let transform_aux t p = 
  List.map (function a ->
              { a with sa=Cairo.Matrix.transform_point t a.sa;
                  sb=Cairo.Matrix.transform_point t a.sb;
                  sc=Cairo.Matrix.transform_point t a.sc;
                  sd=Cairo.Matrix.transform_point t a.sd}) p

let transform t p = 
  {p with pl= transform_aux t p.pl}
                      
let buildcycle p1 p2 = not_implemented ("buildcycle")

let close p1 = 
  if true (* TODO: tester si il est fermé*) then
    {p1 with cycle = true}
  else invalid_arg ("This path is not closed")

let of_bounding_box (x_min,y_min,x_max,y_max) =
  let ul = {x=x_min;y=y_min} in
  let ur = {x=x_max;y=y_min} in
  let dl = {x=x_min;y=y_max} in
  let dr = {x=x_max;y=y_max} in
  close (create_lines [ul;ur;dl;dr;ul])

let length p = max_abscissa p -. min_abscissa p

module Epure =
  struct
    (* A rendre plus performant ou pas*)
    type t = spline list list
    let empty = []
    let create x = [x.pl]
    let of_path = create
    let union x y = List.rev_append x y
    let transform t x = List.map (fun x -> transform_aux t x) x
    let bounding_box sl =
      let (x_min,y_min,x_max,y_max) = 
        list_min_max (list_min_max give_bound_precise) sl in
          ({x=x_min;y=y_min},{x=x_max;y=y_max})
    let of_bounding_box l = create (of_bounding_box l)
  end



module Approx =
  struct
    let lineto l = not_implemented "lineto"
    let fullcircle () = not_implemented "circle"
    let halfcirle () = not_implemented "halfcircle"
    let quartercircle () = not_implemented "quartercircle"
    let unitsquare () = not_implemented "unit square"
  end

module Metapath =
 struct
   type joint =
     | JLine
     | JCurve
     | JCurveNoInflex
     | JTension of float * float
     | JControls of point * point

   type direction =
     | DVec of point
     | DCurl of float
     | DNo

   type knot = direction * point * direction

   type t =
     | Start of knot
     | Cons of t * joint * knot
     | Start_Path of path
     | Append_Path of t * joint * path

   let knot d1 p d2 = (d1,p,d2)
   
   let vec_direction p = DVec p
   let curl_direction f = DCurl f
   let no_direction = DNo

   let start k = Start k
   
   let line_joint = JLine
   let curve_joint = JCurve
   let curve_no_inflex_joint = JCurveNoInflex
   let tension_joint f1 f2 = JTension (f1,f2)
   let controls_joint p1 p2 = JControls (p1,p2)
   

   let concat p j k = Cons (p,j,k)
   let rec append p j = function
     | Start knot -> Cons (p,j,knot)
     | Cons(p2,j2,k2) -> Cons(append p j p2,j2,k2)
     | Start_Path p2 -> Append_Path(p,j,p2)
     | Append_Path (p2,j2,p3) -> Append_Path(append p j p2,j2,p3)


   let to_path p = not_implemented "to_path"
   let cycle d j p = not_implemented "cycle"

   let from_path p = Start_Path p
 end

module Cairo =
  struct
    let draw cr p = 
      Cairo.save cr ;
      List.iter (function 
                   | {start = true} as s -> 
                       Cairo.move_to cr s.sa.x s.sa.y ;
                       Cairo.curve_to cr 
                         s.sb.x s.sb.y 
                         s.sc.x s.sc.y 
                         s.sd.x s.sd.y
                   | s -> 
                       Cairo.curve_to cr 
                         s.sb.x s.sb.y 
                         s.sc.x s.sc.y 
                         s.sd.x s.sd.y) p.pl;
      if p.cycle then Cairo.close_path cr;
      Cairo.restore cr
  end


(*
  module Splines_lib_knuth = 
  struct
(** Knuth solution (with some simplifications) *)
  type red_bez = { u1 : point;
  u2 : point;
  u3 : point;
  umax : point;
  umin : point;
  ut : float}
  let min_max v1 v2 v3 =
  let aux u1 u2 u3 =
  if u1<0. then
  if u3>=0. then
  (if u2 < 0. then u1+.u2 else u1),max 0. (u1+.u2+.u3)
  else
  (min u1 (u1+.u2+.u3),max 0. u1+.u2)
  else
  if u3<=0. then
  (min 0. (u1+.u2+.u3),if u2>0. then u1+.u2 else u1)
  else
  (min 0. (u1+.u2),max u1 (u1+.u2+.u3)) in
  let (xmin,xmax) = aux v1.x v2.x v3.x in
  let (ymin,ymax) = aux v1.y v2.y v3.y in
  ({x=xmin;y=ymin},{x=xmax;y=ymax})


  let leq {x=xa;y=ya} {x=xb;y=yb} = xa <= xb && ya <= yb

  let leqleq a b c = leq a b && leq b c

  let bisect2 w dt = (* 559 *)
  let w1l = w.u1 in
  let w3r = w.u3 in
  let w2l = add_half w1l w.u2 in
  let w2r = add_half w3r w.u2 in
  let w3l = add_half w2l w2r in
  let w1r = w3l in
  let (wminl,wmaxl) = min_max w1l w2l w3l in
  let (wminr,wmaxr) = min_max w1r w2r w3r in
  ({u1 = w1l;u2 = w2l; u3 = w3l; umax = wmaxl;umin=wminl; ut = w.ut},
  {u1 = w1r;u2 = w2r; u3 = w3r; umax = wmaxr;umin=wminr; ut = w.ut +. dt})

  let cubic_intersection p pp =(*550*)
  let rec aux w z d l n dt =(*556*)
  if not (leqleq (minus z.umin w.umax) d (minus z.umax w.umin))
  then l
  else if n=0 then
  (w.ut,z.ut)::l
  else
(* 559 *)
  let n = n-1 in
  let dt = dt/.2. in
  let (wl,wr) = bisect2 w dt in
  let (zl,zr) = bisect2 z dt in
  let dll = double d in
  let l = aux wl zl dll l n dt in
  let drl = add (add (add dll wl.u1) wl.u2) wl.u3 in
  let l = aux wr zl drl l n dt in
  let drr = minus (minus (minus drl zl.u1) zl.u2) zl.u3 in
  let l = aux wr zr drr l n dt in
  let dlr = minus (minus (minus dll zl.u1) zl.u2) zl.u3 in
  let l = aux wl zr dlr l n dt in
  l
  in
(*558*)
  let w1 = minus p.(1) p.(0) in
  let w2 = minus p.(2) p.(1) in
  let w3 = minus p.(3) p.(2) in
  let (wmin,wmax) = min_max w1 w2 w3 in
  let w = {u1=w1;u2=w2;u3=w3;umin=wmin;umax=wmax;ut=0.} in
  let z1 = minus p.(1) p.(0) in
  let z2 = minus p.(2) p.(1) in
  let z3 = minus p.(3) p.(2) in
  let (zmin,zmax) = min_max z1 z2 z3 in
  let z = {u1=z1;u2=z2;u3=z3;umin=zmin;umax=zmax;ut=0.} in
  let d = minus p.(0) pp.(0) in
  aux w z d [] (!inter_depth) 1.
*)
(** *)

