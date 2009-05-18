open Format
exception Not_implemented of string

let not_implemented s = raise (Not_implemented s)

module P = Point_lib
type point = P.point

let id x = x
open Point_lib
open Point_lib.Infix

let rec one_to_one2 f acc a b =
  List.fold_left 
    (fun acc ea -> 
      List.fold_left (fun acc eb -> f acc ea eb) acc b) 
    acc a

let inter_depth = ref 15
let debug = false
let info = false

type abscissa = float

type spline = {sa : point;
               sb : point;
               sc : point;
               sd : point;
               smin : abscissa;
               smax : abscissa;
               start : bool}

type path_ = {pl : spline list;
             cycle : bool}

type path = | Point of point
            | Path of path_



let pt_f fmt p =
  Format.fprintf fmt "{@[ %.20g,@ %.20g @]}" p.x p.y

let print_spline fmt pt =
  Format.fprintf fmt "@[%f|%f { %a,@ %a,@ %a,@ %a }@]@." 
    pt.smin pt.smax pt_f pt.sa pt_f pt.sb pt_f pt.sc pt_f pt.sd

let print_splines = Misc.print_list Misc.semicolon print_spline

let print fmt = function 
  | Point p -> fprintf fmt "@[Point %a@]" P.print p
  | Path p -> fprintf fmt "@[cycle : %b; %a@]" p.cycle print_splines p.pl

let printf = print

let s_of_01 s t = t *. (s.smax -. s.smin) +. s.smin
let _01_of_s s t = (t -. s.smin) /. (s.smax -. s.smin)

let create_point p = Point p

let create_spline ?(start=false) ?(min=0.) ?(max=1.) a b c d = 
  { sa = a; sb = b;
    sc = c; sd = d;
    smin = min; smax = max;
    start = start
  }

let create_with_offset ?start offs a b c d =
  create_spline ?start ~min:offs ~max:(offs +. 1.) a b c d

let create a b c d = 
  Path {pl = [create_spline ~start:true a b c d ]; cycle = false}

let create_line a d = create a a d d 

let create_lines = function 
  | [] -> assert false
  | [a] -> Point a
  | l -> 
      let rec aux = function
        | [] |[_]-> []
        | a::(d::_ as l) -> 
            (create_spline a a d d) :: aux l
      in
      match aux l with
      | [] -> assert false
      | a::l -> Path {pl={a with start = true}::l;cycle=false}


let min_abscissa = function
  | Path p -> (List.hd p.pl).smin
  | Point _ -> 0.

let max_abscissa = function
  | Path p ->
      let rec aux = function
        | [] -> assert false
        | [a] -> a.smax
        | a::l -> aux l in
      aux p.pl
  | Point _ -> 0.

let last_point = function
  | Path p -> (List.hd (List.rev p.pl)).sa
  | Point p -> p

let with_last f p = 
  let rec aux = function
    | [] -> assert false
    | [{sc=sc; sd = sd; smax = smax} as e] -> [ e; f sc sd smax]
    | a::l -> a::(aux l) 
  in
  {p with pl = aux p.pl}

let add_end p c d =
  match p with
  | Point p -> create p c c d
  | Path p ->
      Path (with_last 
        (fun mb a smax -> create_with_offset smax a (2. */ a -/ mb) c d) p)

let add_end_line p d =
  match p with
  | Point p -> create_line p d
  | Path p ->
      Path (with_last (fun mb a smax -> create_with_offset smax a a d d) p)

let add_end_spline p sb sc d =
  match p with
  | Point p -> create p sb sc d
  | Path p ->
      Path (with_last (fun mb a smax -> create_with_offset smax a sb sc d) p)


let f4 f a b c d = f (f a b) (f c d)

let cubic a b c d t =
  t*.(t*.(t*.(d +. 3.*.(b-.c) -. a)+.3.*.(c -. (2.*.b) +. a))+.3.*.(b -. a))+.a
    (*  ((t^3)*(d - (3*c) + (3*b) - a)) + (3*(t^2)*(c - (2*b) + a)) + (3*t*(b - a)) + a*)
    (*  d*.(t**3.)+.3.*.c*.(t**2.)*.(1.-.t)+.3.*.b*.(t**1.)*.(1.-.t)**2.+.a*.(1.-.t)**3.*)

let cubic_point s t =
  {x=cubic s.sa.x s.sb.x s.sc.x s.sd.x t;
   y=cubic s.sa.y s.sb.y s.sc.y s.sd.y t;}

let cubic_point_s s t = cubic_point s (_01_of_s s t)

let abscissa_to_point p0 t = 
  match p0 with
    | Path p ->
        let rec aux = function
          |[] -> invalid_arg "abscissa_to_point : the abscissa given is greater than max_abscissa" 
          | a::l when a.smax >= t -> cubic_point_s a t
          | _::l -> aux l in
        if min_abscissa p0 > t then 
          invalid_arg "abscissa_to_point : the abscissa given is smaller than min_abscissa"
        else aux p.pl
    | Point p when t = 0. -> p
    | Point _ -> invalid_arg "abscissa_to_point : a point has only the abscissa 0."


let direction_of_abscissa_aux s t = 
  (*Format.printf "s = %a; t = %f@." print_spline s t;*)
  (t**2.) */ s.sd +/ (((2. *. t) -. (3. *. (t**2.)))) */ s.sc +/ 
  ((1. -. (4. *. t)+.(3. *. (t**2.)))) */ s.sb +/ (-.((1. -. t)**2.)) */ s.sa
(* le // 3 est pour la comp avec metapost *)

let direction_of_abscissa p0 t = 
  match p0 with
    | Point _ -> invalid_arg "direction_of_abscissa : a point has no direction."
    | Path p ->  
        let rec aux = function
          |[] -> invalid_arg "direction_of_abscissa : the abscissa given is greater than max_abscissa" 
          | a::_ when a.smax >= t -> direction_of_abscissa_aux a (_01_of_s a t)
          | _::l -> aux l in
        if min_abscissa p0 > t then 
          invalid_arg "direction_of_abscissa : the abscissa given is smaller than min_abscissa"
        else aux p.pl
        

let extremum conv l a b c d =
  let test s l = if s>=0. && s<=1. then (conv s)::l else l in
  let sol delta = (delta -. (2.*.b) +. a +. c)/.(a -. d +. (3.*.(c -. b))) in
  let delta = ((b*.b) -. (c*.(b +. a -. c)) +. (d*.(a -. b)))**0.5 in
  match compare delta 0. with
    | x when x<0 -> []
    | 0 -> test (sol 0.) l
    | _ -> test (sol delta) (test (sol (-.delta)) l)

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
                     max x_max sx_max,max y_max sy_max))
    (infinity,infinity,neg_infinity,neg_infinity) p

let unprecise_bounding_box = function
  | Path s -> 
      let (x_min,y_min,x_max,y_max) = 
        list_min_max give_bound s.pl in
      ({x=x_min;y=y_min},{x=x_max;y=y_max})
  | Point s -> (s,s)

let give_bound_precise s =
      let x_remarq = List.map (cubic s.sa.x s.sb.x s.sc.x s.sd.x) (remarquable id [] s.sa.x s.sb.x s.sc.x s.sd.x) in
      let y_remarq = List.map (cubic s.sa.y s.sb.y s.sc.y s.sd.y) (remarquable id [] s.sa.y s.sb.y s.sc.y s.sd.y) in
      let x_max = List.fold_left max neg_infinity x_remarq in
      let y_max = List.fold_left max neg_infinity y_remarq in
      let x_min = List.fold_left min infinity x_remarq in
      let y_min = List.fold_left min infinity y_remarq in
      (x_min,y_min,x_max,y_max)

    
let bounding_box = function
  | Path s ->
      let (x_min,y_min,x_max,y_max) = 
        list_min_max give_bound_precise s.pl in
      ({x=x_min;y=y_min},{x=x_max;y=y_max})
  | Point s -> (s,s)

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
  match a,b with
    | Path a,Path b ->
        (try
          one_to_one2 one_intersection_aux () a.pl b.pl;
           if debug then
             (Format.printf "one_intersection : Not_found use 0.,0.@.@?";
              0.,0.)
           else
             raise Not_found
        with
            Found (t1,t2) -> (t1,t2))
    | _ -> if debug then
             Format.printf "one_intersection : Not_found not two path@.@?";
        raise Not_found

let intersection_aux acc a b =
  if a=b then [] else
    let rec aux a b l t1 t2 dt= function
      | 0 -> if is_intersect a b then (t1+(dt/2),t2+(dt/2))::l else l
      | n -> 
          if debug then
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
    if debug then
      Format.printf "@[%a@]@." (fun fmt -> List.iter (pt_f fmt))
        (List.map (fun (t1,t2) -> (cubic_point a t1) -/ (cubic_point b t2)) res);
    res


let intersection a b = 
  match a,b with
    | Path a,Path b -> one_to_one2 intersection_aux [] a.pl b.pl
    | _ -> []

let fold_left f acc = function
  | Path p -> List.fold_left (fun acc s -> f acc s.sa s.sb s.sc s.sd) acc p.pl
  | Point _ -> acc
  
let iter f = function
  | Path p ->  List.iter (fun s -> f s.sa s.sb s.sc s.sd) p.pl
  | Point _ ->  ()

let union_conv ap bp = 
  let max = max_abscissa ap in
  let min = min_abscissa bp in
  let diff = max-.min in
  (fun x -> x +. diff)
 
(*
let union ap bp = 
  let conv = union_conv ap bp in
  {pl=List.map (function {smin=smin;smax=smax} as b -> 
                  {b with smin=conv smin;smax=conv smax}) bp.pl;cycle=false}
*)
let append_conv ap bp = 
  let union_conv = union_conv ap bp in
  (fun x -> (union_conv x)+.1.)

let ext_list = function
  | [] -> assert false
  | a::l -> a,l

let append ap0 sb sc bp0 = 
  match ap0, bp0 with
    | Path ap,Path bp ->
  let conv = append_conv ap0 bp0 in
  let fbpconv,bpconv = 
    ext_list (List.map (function {smin=smin;smax=smax} as b -> 
                          {b with smin=(conv smin)+.1.;smax=(conv smax)+.1.})
                bp.pl) in
  let rec aux = function
    |[] -> assert false
    |[{smax=smin;sd=sa} as a] -> 
       a::{smin=smin;smax=smin+.1.;
           sa=sa;sb=sb;sc=sc;sd=fbpconv.sa;start=false}::
         {fbpconv with start=false}::bpconv
    |a::l -> a::(aux l) in
  Path {pl=aux ap.pl;cycle=false}
    | _ -> raise (Not_implemented "autre cas de append") (* TODO *)

let reverse = function 
  | Path p as p0 ->
      let conv = 
        let max = max_abscissa p0 in
        let min = min_abscissa p0 in
        let sum = max +. min in
        (fun x -> sum -. x) in
      let rec aux acc = function
        | [] -> acc
        | ({sa=sa;sd=sd;smin=smin;smax=smax} as a)::l -> 
            aux ({a with sa=sd;sd=sa;
                    smin=conv smin; smax=conv smax}::acc) l in
      Path {p with pl = aux [] p.pl}
  | Point _ as p -> p

(*left ((t^3)*(d + (3*(b - c)) - a)) + ((t^2)*(d - (3*b) + (2*a))) + (t*((2*c) - b - a)) + b *)
(*right 3*d - c *)
let cast_path_to_point p = function
  | Path {pl=[];} -> Point p
  | x -> x

let split_aux s t l = 
  if t = s.smax then ([s],cast_path_to_point s.sd (Path {pl=l;cycle=false}))
  else if t = s.smin then ([],Path {pl=s::l;cycle=false})
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
     Path {pl={s with sa = a2;sb = b2;sc = c2;smin = t}::l;cycle=false})

let split p0 t = 
  match p0 with
    | Path p ->
        let rec aux = function
          |[] -> invalid_arg "split : the abscissa given is greater than max_abscissa" 
          | a::l when a.smax > t -> split_aux a t l
          | a::l -> let (p1,p2) = aux l in (a::p1,p2) in
        if min_abscissa p0 > t then 
          invalid_arg "split : the abscissa given is smaller than min_abscissa"
        else 
          let (p1,p2) = aux p.pl in
          (cast_path_to_point (List.hd p.pl).sa (Path {pl=p1;cycle = false}),p2)
    | Point _ when t = 0. -> p0,p0
    | Point _ -> invalid_arg "split : a point can be split only at 0."
      
let subpath p t1 t2 = fst (split (snd (split p t1)) t2)

let cut_before a b = 
  if debug then
    Format.printf "Spline_lib.cut_before : a = %a; b = %a" print a print b;
  fst (split a (fst (one_intersection a b)))
let cut_after a b = 
  if debug then
    Format.printf "Spline_lib.cut_after : a = %a; b = %a" print a print b;
  reverse (fst (split a (fst (one_intersection (reverse a) b))))

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
    
let dist_min_point p point = 
  match p with
    | Path p ->
        snd (List.fold_left (dist_min_point_aux point)
               (let one = (List.hd p.pl) in (norm2 (one.sa.x -. point.x) (one.sa.y -. point.y),one.smin)) p.pl)
    | Point p -> 0.

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

let dist_min_path p1 p2 = 
  match p1, p2 with
    | Path p1, Path p2 ->
        snd (one_to_one2 dist_min_path_aux
               (let one1 = (List.hd p1.pl) in 
                let one2 = (List.hd p2.pl) in 
                (norm2 (one1.sa.x -. one2.sa.x) (one1.sa.y -. one2.sa.y),(one1.smin,one2.smin))) p1.pl p2.pl)
    |Path _ as p1, Point p2 -> dist_min_point p1 p2,0.
    |Point p1, (Path _ as p2) -> 0.,dist_min_point p2 p1
    |Point _, Point _ -> 0.,0.

let translate p t = 
  match p with
    | Path p ->
        Path {p with pl=List.map (function a ->
                               { a with sa= a.sa +/ t;
                                   sb=a.sb +/ t;
                                   sc=a.sc +/ t;
                                   sd=a.sd +/ t}) p.pl}
    | Point p -> Point (p +/ t)


let transform_aux t p = 
  List.map (function a ->
              { a with sa=Cairo.Matrix.transform_point t a.sa;
                  sb=Cairo.Matrix.transform_point t a.sb;
                  sc=Cairo.Matrix.transform_point t a.sc;
                  sd=Cairo.Matrix.transform_point t a.sd}) p

let transform t = function
  | Path p -> Path {p with pl= transform_aux t p.pl}
  | Point p -> Point (P.transform t p)
    
let buildcycle p1 p2 = not_implemented ("buildcycle")

let close = function
  | Path p1 (* TODO: tester si il est fermé*) ->
      Path {p1 with cycle = true}
  | Point _ -> invalid_arg ("This path is not closed")

let of_bounding_box ({x=x_min;y=y_min},{x=x_max;y=y_max}) =
  let dl = {x=x_min;y=y_min} in
  let dr = {x=x_max;y=y_min} in
  let ul = {x=x_min;y=y_max} in
  let ur = {x=x_max;y=y_max} in
  close (create_lines [ul;ur;dr;dl;ul])

let length p = max_abscissa p -. min_abscissa p

module Metapath =
struct
  type direction =
    | DVec of point
    | DCurl of float
    | DNo

  type joint =
    | JLine
    | JCurve of direction * direction
    | JCurveNoInflex  of direction * direction
    | JTension of direction * float * float * direction
    | JControls of point * point

  type knot = point

  type t =
    | Start of knot
    | Cons of t * joint * knot
    | Start_Path of spline list
    | Append_Path of t * joint * (spline list)


  let rec print_dir fmt = function
    |DNo -> fprintf fmt "DNo"
    |DVec p -> fprintf fmt "DVec %a" Point_lib.print p
    |DCurl f -> fprintf fmt "Dcurl %f" f
  and print_knot = Point_lib.print
  and print_joint fmt = function
    | JLine -> fprintf fmt "JLine"
    | JCurve _ -> fprintf fmt "JCurve"
    | JCurveNoInflex _ -> fprintf fmt "JCurveNoInflex"
    | JTension (_,f1,f2,_) -> fprintf fmt "JTension (%f,%f)" f1 f2
    | JControls (p1,p2) -> fprintf fmt "JControls (%a,%a)" Point_lib.print p1 Point_lib.print p2
  and print fmt = function
    | Start k1 -> fprintf fmt "[%a" print_knot k1
    | Cons (p,j,k) -> fprintf fmt "%a;%a-%a" print p print_joint j print_knot k
    | Start_Path p-> fprintf fmt "{%a}" print_splines p
    | Append_Path (p1,j,p2) ->fprintf fmt "%a;%a-%a" print p1 print_joint j print_splines p2


  (* Metafont is wiser in the computation of 
     calc_value, calc_ff, curl_ratio, ... *)
  (* dk1, uk1 are d_k-1, u_k-1 *)
  let curl_ratio gamma alpha1 beta1 =
    let alpha = 1./.alpha1 in
    let beta = 1./.beta1 in
    ((3.-.alpha)*.alpha*.alpha*.gamma+.beta**3.)/.
      (alpha**3.*.gamma +. (3.-.beta)*.beta*.beta)

  let reduce_angle x = 
    (* 292. define reduce angle (#) *)
    if (abs_float x) > 180. 
    then if x>0. then x -. 360. else x +. 360.
    else x

  let velocity st ct sf cf t = 
    let num = 2.+.(sqrt 2.)*.(st -. (sf /. 16.))*.(sf -. (st /. 16.))*.(ct-.cf) in
    let denom = 3.*.(1.+.0.5*.((sqrt 5.) -. 1.)*.ct +. 0.5 *.(3.-.(sqrt 5.))*.cf) in
    min ((t*.num)/.denom) 4.

  let calc_value dk1 dk absrtension absltension uk1 =
    (* Calculate the values aa = Ak /Bk , bb = Dk /Ck , dd = (3 - k-1 )dk,k+1 , ee = (3 - k+1 )dk-1,k , and
     cc = (Bk - uk-1 Ak )/Bk 288 *)
    let aa = 1./.(3.*.absrtension -. 1.) in
    let bb = 1./.(3.*.absltension -. 1.) in
    let dd = dk*.(3.-.1./.absrtension) in
    let ee = dk1*.(3.-.1./.absltension) in
    let cc = 1.-.(uk1*.aa) in
    (aa,bb,cc,dd,ee)
      
  let calc_ff cc dd ee absrtension absltension =
    (* Calculate the ratio ff = Ck /(Ck + Bk - uk-1 Ak ) 289 *)
    (*let dd = dd*.cc in
    if absltension = absrtension 
    then ee/.(ee+.dd)
    else if absltension < absrtension
    then 
      let tmp = absltension/.absrtension in
      let tmp = tmp*.tmp in
      let dd = dd*.tmp in
      ee/.(ee+.dd)
    else
      let tmp = absrtension/.absltension in
      let tmp = tmp*.tmp in
      let ee = ee*.tmp in
      ee/.(ee+.dd)*)
    let dd = dd*.cc in
    let tmp = absltension/.absrtension in
    let tmp = tmp*.tmp in
    let dd = dd*.tmp in
    ee/.(ee+.dd)

  type tension = float
      
  type path_type = 
    | Endpoint
    | Explicit of point
    | Open of tension
    | Endcycle of tension
    | Curl of tension * float
    | Given of tension * float

  let tension = function
    | Endpoint -> 1. (* not sure ... *)
    | Explicit _ -> assert false
    | Open t | Endcycle t | Curl (t,_) | Given (t,_) -> t

  type kpath = {
    mutable left : path_type;
    mutable right : path_type;
    mutable link : kpath;
    mutable coord : point}

  let print_path_type fmt = function
    | Endpoint -> fprintf fmt "Endpoint"
    | Explicit p -> fprintf fmt "Explicit %a" P.print p
    | Open t -> fprintf fmt "Open %f" t
    | Endcycle t -> fprintf fmt "Endcycle %f" t
    | Curl (t,f) -> fprintf fmt "Curl (%f,%f)" t f
    | Given (t,f) -> fprintf fmt "Given (%f,%f)" t f

  let print_one_kpath fmt q =
    fprintf fmt "@[{left = @[%a@];@,coord = @[%a@];@,right = @[%a@]}@]"
          print_path_type q.left
          P.print q.coord
          print_path_type q.right

  let print_kpath fmt p = 
    let rec aux fmt q =
        fprintf fmt "@[{left = @[%a@];@,coord = @[%a@];@,right = @[%a@];@,link= @[%a@]}@]"
          print_path_type q.left
          P.print q.coord
          print_path_type q.right
          (fun fmt q -> 
             if p!=q 
             then aux fmt q
             else fprintf fmt "...") q.link in
    aux fmt p

  let tunity:tension = 1.

  let pi = acos (-.1.)

  let n_arg = 
    let coef = 180./.pi in
    fun x y -> (atan2 y x)*.coef

  let sincos = 
    let coef = pi/.180. in
    fun a -> let a = coef *. a in
    sin a, cos a

  let set_controls p q at af rtension ltension deltaxy =
    (* procedure set controls (p, q : pointer ; k : integer ) 299 *)
    let st,ct = sincos at in
    let sf,cf = sincos af in
    let rr = velocity st ct sf cf (abs_float rtension) in
    let ss = velocity sf cf st ct (abs_float ltension) in
    if (rtension < 0.) && (ltension < 0.) then
      (*TODO*) ();
    let sbp = (rr*/ ((ct */ deltaxy) +/ 
                                 (st*/(P.swapmy deltaxy)))) in
    let sb = p.coord +/  sbp in
    p.right <- Explicit sb;
    let scm = (ss*/((cf */ deltaxy) +/ 
                                (sf*/(P.swapmx deltaxy)))) in
    let sc = q.coord -/ scm in
    q.left <- Explicit sc
    
  let print_array print fmt =
    Array.iter (fun e -> fprintf fmt "%a;@," print e)

  let print_float fmt = fprintf fmt "%f"

  let solve_choices p q n deltaxyk deltak psik =
    (* If Only one simple arc*)
    match p with
      | {right = Given (rt,rp);link={left=Given (lt,lq)}} -> 
          (* Reduce to simple case of two givens and return 301 *)
          let aa = n_arg deltaxyk.(0).x deltaxyk.(0).y in
          let at = rp -. aa in let af = lq -. aa in
          set_controls p q at af rt lt deltaxyk.(0)
      | {right = Curl (tp,cp);link={left=Curl (tq,cq)}} -> 
          (* Reduce to simple case of straight line and return 302 *)
          let lt = abs_float tq in let rt = abs_float tp in
          let tmp = 
            {x=if deltaxyk.(0).x >= 0. then 1. else -.1.;
             y=if deltaxyk.(0).y >= 0. then 1. else -.1.} in
          p.right <- Explicit  
            begin
              if rt = tunity then 
                p.coord +/ ((deltaxyk.(0) +/ tmp)// 3.)
              else
                let ff = 1. /. (3. *. rt) in
                p.coord +/ (deltaxyk.(0) // ff)
            end;
          q.left <- Explicit  
            begin
              if lt = tunity then 
                q.coord +/ ((deltaxyk.(0) +/ tmp)// 3.)
              else
                let ff = 1. /. (3. *. rt) in
                q.coord +/ (deltaxyk.(0) // ff)
            end
      | {link=t} as s -> 
          let thetak = Array.make (n+2) 0. in
          let uu = Array.make (n+1) 0. in
          let vv = Array.make (n+1) 0. in
          let ww = Array.make (n+1) 0. in
          begin
            if debug then
              Format.printf "Open : @[k : @[%i@];@,s : @[%a@]@]@." 0 print_one_kpath p;
            match p with
              |{right=Given (_,rp)} ->
                 (* Set up the equation for a given value of 0 293 *)
                 vv.(0) <- reduce_angle (rp -. (n_arg deltaxyk.(0).x deltaxyk.(0).y));
                  uu.(0) <- 0.;ww.(0) <- 0.
              |{right=Curl (tp,cc);link={left=lt}} ->
                 (* Set up the equation for a curl at 0 294 *)
                 let lt = abs_float (tension lt) in let rt = abs_float tp in
                 if lt = tunity && rt = tunity 
                 then uu.(0) <- (2.*.cc+.1.)/.(cc+.2.)
                 else uu.(0) <- curl_ratio cc rt lt;
                 vv.(0) <- -. (psik.(1)*.uu.(0));
                 ww.(0) <- 0.
              |{right=Open _} ->
                 uu.(0) <- 0.;vv.(0) <- 0.;ww.(0) <- 1.
              | _ -> assert false (* { there are no other cases } in 285 because of 273 *)
          end;
          (let rec aux k r = function
              (* last point*)
            | {left=Curl (t,cc)} -> 
                (* Set up equation for a curl at n and goto found 295 *)
                let lt = abs_float t in
                let rt = abs_float (tension r.right) in
                let ff = if lt=1. && rt=1. 
                then (2.*.cc+.1.)/.(cc+.2.)
                else curl_ratio cc lt rt in
                thetak.(n) <- -.((vv.(n-1)*.ff)/.(1.-.ff*.uu.(n-1)))
            | {left=Given (_,f)} -> 
                (* Calculate the given value of n and goto found 292 *)
                thetak.(n) <- reduce_angle (f -. (n_arg deltaxyk.(n-1).x deltaxyk.(n-1).y))
            | {link=t} as s-> 
                  (*end cycle , open : Set up equation to match mock curvatures at zk ;
                    then goto found with n adjusted to equal 0 , if a cycle has ended 287 *)
                if debug then
                  Format.printf "Open : @[k : @[%i@];@,s : @[%a@]@]@." k print_one_kpath s;
                let absrtension = abs_float (tension r.right) in
                let absltension = abs_float (tension t.left) in
                let (aa,bb,cc,dd,ee) = calc_value deltak.(k-1) deltak.(k) 
                  absrtension absltension uu.(k-1) in
                let ff = calc_ff cc dd ee absrtension absltension in
                uu.(k)<- ff*.bb;
                (* Calculate the values of vk and wk 290 *)
                let acc = -. (psik.(k+1)*.uu.(k)) in
                (match r.right with 
                  | Curl _ -> (*k=1...*) ww.(k) <- 0.; 
                      vv.(k) <- acc -. (psik.(1) *. (1. -. ff))
                  | _ -> 
                      let ff = (1. -. ff)/. cc in
                      let acc = acc -. (psik.(k) *. ff) in
                      let ff = ff*.aa in
                      vv.(k) <- acc -. (vv.(k-1)*.ff);
                      ww.(k) <- -. (ww.(k-1)*.ff));
                (match s.left with
                   | Endcycle _ -> 
                       (* Adjust n to equal 0 and goto found 291 *)
                       let aa,bb = 
                         (let rec aux aa bb = function
                            | 0 -> (vv.(n) -. (aa*.uu.(n))),(ww.(n) -. (bb*.uu.(n)))
                            | k -> aux (vv.(k) -. (aa*.uu.(k))) (ww.(k) -. (bb*.uu.(k))) (k-1) in
                          aux 0. 1. (n-1)) in
                       let aa = aa /. (1. -. bb) in
                       thetak.(n) <- aa;
                       vv.(0) <- aa;
                       for k = 1 to n-1 do
                         vv.(k) <- vv.(k) +. (aa*.ww.(k));
                       done;
                   | _ -> aux (k+1) s t);
           in aux 1 s t);
          (* Finish choosing angles and assigning control points 297 *)
          if debug then
            Format.printf "Finish choosing angles and assigning control points 297@.";
          for k = n-1 downto 0 do
            thetak.(k) <- vv.(k) -. (thetak.(k+1) *. uu.(k))
          done;
          (*Format.printf "thetak : %a@." (print_array print_float) thetak;
          Format.printf "uu : %a@." (print_array print_float) uu;
          Format.printf "vv : %a@." (print_array print_float) vv;
          Format.printf "ww : %a@." (print_array print_float) ww;*)
          (let rec aux k = function
             | _ when k = n -> ()
             | {right=rt;link={left=lt} as t} as s ->
                 let at = thetak.(k) in
                 let af = -.psik.(k+1) -. thetak.(k+1) in
                 (*Format.printf "Set_controls before : @[%a ." print_one_kpath s;*)
                 set_controls s t at af (tension rt) (tension lt) deltaxyk.(k);
                 (*Format.printf "Set_controls after : @[%a@]@." print_one_kpath s;*)
                 aux (k+1) t
           in aux 0 p)

  let make_choices knots =
    (* If consecutive knots are equal, join them explicitly 271*)
    (let p = ref knots in
    while !p != knots do
      (match !p with
        | ({coord=coord;right=(Given _|Curl _|Open _);link=q} as k) when coord == q.coord ->
            if debug then
              Format.printf "@[find consecutive knots :k = @[%a@];@,q = @[%a@]@]@."
                print_one_kpath k print_one_kpath q;
            k.right <- Explicit coord;
            q.left  <- Explicit coord;
            (match k.left with
              | Open tension -> k.left <- Curl (tension,tunity)
              | _ -> ());
            (match k.right with
              | Open tension -> k.right <- Curl (tension,tunity)
              | _ -> ());
        | _ -> ());
      p:=(!p).link;
    done);
    (*Find the first breakpoint, h, on the path; insert an artificial breakpoint if the path is an unbroken
      cycle 272*)
    let h = 
      (let rec aux = function
         | {left = (Endpoint | Endcycle _ | Explicit _ | Curl _ | Given _)}
         | {right= (Endpoint | Endcycle _ | Explicit _ | Curl _ | Given _)} as h -> h
         | {left = Open t} as h when h==knots -> knots.left <- Endcycle t; knots
         | {link=h} -> aux h in
       aux knots) in
    if debug then 
      Format.printf "@[find h :h = @[%a@]@]@."
        print_one_kpath h;
    (*repeat Fill in the control points between p and the next breakpoint, then advance p to that
      breakpoint 273
      until p = h*)
  (let rec aux = function
     | {right =(Endpoint|Explicit _);link = q} -> if q!=h then aux q
     | p -> let n,q = 
         (let rec search_q n = function
            |{left=Open _;right=Open _;link=q} -> search_q (n+1) q
            | q -> (n,q) in
          search_q 1 p.link) in
       if debug then
         Format.printf "@[search_q : n = %i;@,p = @[%a@];@,q = @[%a@]@]@."
           n print_one_kpath p print_one_kpath q;
       (*Fill in the control information between consecutive breakpoints p and q 278*)
       (*  Calculate the turning angles k and the distances dk,k+1 ; set n to the length of the path 281*)
       let deltaxyk = Array.make (n+1) P.zero in
       (* Un chemin sans cycle demande un tableau de taille n, de n+1 avec cycle *)
       let deltak = Array.make (n+1) 0. in
       let psik = Array.make (n+2) 0. in
       (let rec fill_array k = function
            (* K. utilise des inégalitées pour k=n et k = n+1 -> k>=n*)
          | s when k = n && (match s.left with |Endcycle _ -> false | _ -> true) -> psik.(n) <- 0. 
              (* On a fait un tour le s.left précédent était un Endcycle *) 
          | _ when k = n+1 -> psik.(n+1)<-psik.(1) 
          | {link=t} as s ->
              deltaxyk.(k) <- t.coord -/ s.coord;
              deltak.(k) <- P.norm deltaxyk.(k);
              (if k > 0 then
                 let {x=cosine;y=sine} = deltaxyk.(k-1) // deltak.(k-1) in
                 let psi = n_arg (deltaxyk.(k).x*.cosine+.deltaxyk.(k).y*.sine)
                   (deltaxyk.(k).y*.cosine-.deltaxyk.(k).x*.sine) in
                 psik.(k) <- psi);
              fill_array (k+1) t
        in fill_array 0 p);
       (*Format.printf "deltaxyk : %a@." (print_array P.print) deltaxyk;
       Format.printf "deltak : %a@." (print_array print_float) deltak;
       Format.printf "psik : %a@." (print_array print_float) psik;*)
       (*Remove open types at the breakpoints 282*)
       (match q with
         | {left=Open t} -> q.left <- Curl (t,1.) (* TODO cas bizarre *)
         | _ -> ());
       (match p with
          | {left=Explicit pe;right=Open t} -> 
             let del = p.coord -/ pe in
             if del=P.zero 
             then p.right <- Curl (t,1.)
             else p.right <- Given (t,n_arg del.x del.y)
          | _ -> ());
       (* an auxiliary function *)
         solve_choices p q n deltaxyk deltak psik;
       if q!=h then aux q
   in aux h)
        
  let tension_of = function
    | JTension (_,t1,t2,_) -> (t1,t2)
    | _ -> (1.,1.)
        
  let direction t = function
    | DNo -> Open t
    | DVec p -> Given (t,n_arg p.x p.y)
    | DCurl f -> Curl (t,f)
        
  let right_of_join p = function
    | JLine -> Explicit p
    | JControls (c,_) -> Explicit c
    | JCurve (d,_) -> direction 1. d
    | JCurveNoInflex (d,_) -> direction 1. d (*pas totalement correcte*)
    | JTension (d,f,_,_) -> direction f d
        
  let left_of_join p = function
    | JLine -> Explicit p
    | JControls (_,c) -> Explicit c
    | JCurve (_,d) -> direction 1. d
    | JCurveNoInflex (_,d) -> direction 1. d (*pas totalement correcte*)
    | JTension (_,_,f,d) -> direction f d

 let dumb_pos = {x=666.;y=42.}
 let dumb_dir = Endcycle 42.

 let rec dumb_link = { left = dumb_dir;
                       right = dumb_dir;
                       coord = dumb_pos;
                       link = dumb_link}

  let path_to_meta nknot l =
    let rec aux aknot = function
    | [] -> assert false
    | [{sa=sa;sb=sb;sc=sc;sd=sd}] ->
        nknot.left <- Explicit sc;
        nknot.coord <- sd;
        aknot.link <- nknot ;
        aknot.right <- Explicit sb;
        aknot.coord <- sa; ()
    | {sa=sa;sb=sb;sc=sc}::l -> 
        let nknot = {left = Explicit sc;
                     right = dumb_dir;
                     coord = dumb_pos;
                     link = dumb_link} in
        aknot.link <- nknot;
        aknot.right <- Explicit sb;
        aknot.coord <- sa;
        aux nknot l in
    let aknot = {left = Endpoint;
         right = dumb_dir;
         coord = dumb_pos;
         link = dumb_link} in
    aux aknot l;
    aknot
                     
   let kmeta_to_path ?cycle meta = 
     let rec to_knots aknot = function
       | Start p -> 
           aknot.coord <- p;
           aknot.left <- Endpoint;
           aknot
       | Cons (pa,join,p) ->
           aknot.coord <- p;
           aknot.left <- left_of_join p join;
           let nknot = {right = right_of_join p join;
                        left = dumb_dir;
                        coord = dumb_pos;
                        link = aknot} in
           to_knots nknot pa
       | Start_Path pa -> path_to_meta aknot pa
       | Append_Path (p1,join,p2) -> 
           let aknot2 = path_to_meta aknot p2 in 
           aknot2.left<- left_of_join aknot2.coord join;
           let nknot = {right = right_of_join aknot2.coord join;
                        left = dumb_dir;
                        coord = dumb_pos;
                        link = aknot} in
           to_knots nknot p1 in
     let lknots = {right = Endpoint;
                   left = dumb_dir;
                   coord = dumb_pos;
                   link = dumb_link} in
     let knots = to_knots lknots meta in
     lknots.link <- knots;
     (* Choose control points for the path and put the result into cur exp 891 *)
     (* when nocycle *)
     begin
       match cycle with
         |Some join -> 
            begin
              lknots.right <- right_of_join knots.coord join;
              knots.left <- left_of_join knots.coord join;
            end
         | None ->
             begin 
               (match knots.right with
                  | Open t -> knots.right <- Curl (t,1.)
                  | _ -> ());
               (match lknots.left with
                  | Open t -> lknots.left <- Curl (t,1.)
                  | _ -> ());
             end
     end;
     if debug then
       Format.printf "@[before : @[%a@];@.middle : @[%a@]@]@." print meta print_kpath knots;
     make_choices knots;
     if debug then
       Format.printf "@[after : @[%a@]@]@." print_kpath knots;
     let rec aux smin = function
     | {right = Endpoint} -> []
     | {right = Explicit sb;coord = sa;
        link={left = Explicit sc;coord = sd} as s} ->
         {sa = sa;sb = sb;sc = sc;sd = sd;smin=smin;smax=smin+.1.;start=false}::
           (if s==knots then [] else (aux (smin+.1.) s))
     | _ -> assert false in
      let l = aux 0. knots in
      {(List.hd l) with start = true}::(List.tl l)


         


(*
  let p213 p1 p2 p3 : P.t * P.t =
    let p21 = P.sub p2 p1 in
    let p23 = P.sub p2 p3 in
    (p21,p23)

  let ro p1 p2 p3 = 
    let (p21,p23) = p213 p1 p2 p3 in
    sqrt ((P.norm2 p23)/.(P.norm2 p21))

  let psi p1 p2 p3 =
    let (p21,p23) = p213 p1 p2 p3 in
    (atan2 p23.x p23.y)-.(atan2 p21.x p21.y) (* faux*)

  let deltak p1 p2 = 
    let p21 = P.sub p2 p1 in
    P.norm2 p21


  type delta = float
  and tc = tension * point * tc_aux
  and tc_aux =
    | COpen of tension * tc
    | CEnd of tcend
    | CCycle of tension * tcend * (point * delta * float)
  and tcend =
      [ `Curve of tension * tension * float
      | `Given of point * float ]

  and tcl =
    | TC of (tcend * point * tension ) * tc * tcl
    | TExplicit of spline * tcl
    | TEnd

  type sc_data = 
      {psi1k : float;
       psik : float;
       dk : float;
       dk1 : float;
       deltak :point;
       rtension : float;
       ltension : float;
       pk: point;
       p1k: point;
      }

  type tc_cycle = sc_data list

  let rec calc_angle athetal thetal = function
    | [] -> athetal::thetal
    | (uk,vk,wk)::uvwkl -> 
        let nathetal = vk -. (athetal *. uk) in
        calc_angle nathetal (athetal::thetal) uvwkl


  (* set_controls*)
  let rec calc_control_points smin splines dkpsil thetal= 
    match dkpsil,thetal with
      | [],_ | _,[] | _,[_] -> splines
      | (data::dkpsil),(theta1k::(thetak::_ as thetal)) -> 
          let st,ct = sin thetak,cos thetak in
          let tmp = -.data.psi1k-.theta1k in
          let sf,cf = sin tmp,cos tmp in
          let rr = velocity st ct sf cf (abs_float data.rtension) in
          let ss = velocity st ct sf cf (abs_float data.ltension) in
          if (data.rtension < 0.) && (data.ltension < 0.) then
            (*TODO*) ();
          let sa = data.pk in
          let sb = sa +/ (rr*/ ((ct */ data.deltak) +/ 
                                  (st*/{data.deltak with x= -.data.deltak.x}))) in
          let sc = sa -/ (ss*/((cf */ data.deltak) +/ 
                                 (sf*/{data.deltak with y= -.data.deltak.y}))) in
          let sd = data.p1k in
          calc_control_points (smin+.1.)
            ({sa = sd;
              sb = sc;
              sc = sb;
              sd = sa;
              smin = smin;
              smax = smin+.1.;
              start =false}::splines)
            dkpsil thetal
            
  let solve_choices_cycle smin first tc =
    let rec aux uvwkl = function
      | {ltension=ltension;rtension=rtension;
         pk=pk;p1k=p1k;
         psik=psik;psi1k=psi1k;
         deltak=deltak;
         dk=dk;dk1=dk1}::ltc ->
          let (uk1,vk1,wk1) = List.hd uvwkl in
          let (aa,bb,cc,dd,ee) = calc_value dk1 dk (abs_float rtension) (abs_float ltension) uk1 in
          let ff = calc_ff cc dd ee (abs_float rtension) (abs_float ltension) in
          let uk = ff*.bb in
          (* perhaps should test for curl as in metafont *)
          let acc = -.psi1k*.uk in
          let ff = (1.-.ff)*.cc in 
          let acc = acc -. (psik*.ff) in
          let ff = ff*.aa in
          let vk = acc -. (vk1*.ff) in
          let wk = -.(wk1*.ff) in
          let uvwkl = (uk,vk,wk)::uvwkl in
          aux uvwkl ltc
      | [] ->
          let aa,bb = 
            List.fold_left (fun (aa,bb) (uk,vk,wk) -> vk -. aa*.uk, wk-. bb*.uk)
              (0.,1.) uvwkl in
          let aa = aa/.(1.-.bb) in 
          let thetan = aa in
          (List.map (fun (uk,vk,wk) -> (uk,vk+. aa*.wk,wk)) uvwkl,thetan) in
    let uvwkl =  [0.,0.,1.] in
    let uvwkl,thetan = aux uvwkl tc in
    let thetal = calc_angle thetan [] uvwkl in
    let splines = calc_control_points smin [] tc thetal in
    splines


  let rec solve_choices_nocycle smin sfirst send tc =
    let rec aux uvwkl = function
      | {ltension=ltension;rtension=rtension;
         pk=pk;p1k=p1k;
         psi1k=psi1k;psik=psik;
         deltak=deltak;
         dk=dk;dk1=dk1}::ll ->
          let (uk1,vk1,wk1) = List.hd uvwkl in
          let (aa,bb,cc,dd,ee) = calc_value dk1 dk (abs_float rtension) (abs_float ltension) uk1 in
          let ff = calc_ff cc dd ee (abs_float rtension) (abs_float ltension) in
          let uk = ff*.bb in
          (* perhaps should test for curl as in metafont *)
          let acc = -.psi1k*.uk in
          let ff = (1.-.ff)*.cc in 
          let acc = acc -. (psik*.ff) in
          let ff = ff*.aa in
          let vk = acc -. (vk1*.ff) in
          let wk = -.(wk1*.ff) in
          let uvwkl = (uk,vk,wk)::uvwkl in
          aux uvwkl ll
      | [] -> 
          match send with 
            | `Curve (rt,lt,f) -> 
                let (uk1,vk1,wk1) = List.hd uvwkl in
                let ff = curl_ratio f lt (abs_float rt) in
                let thetan = -. ((vk1*.ff)/.1.-.(ff*.uk1)) in
                (uvwkl,thetan)
            | `Given (deltak1,f) -> 
                let thetan = reduce_angle (f -. (atan2 deltak1.x deltak1.y)) in
                (uvwkl,thetan) in

    (*la première courbe *)
    let uvwkl = 
      let {dk=dk;psi1k=psi1k;deltak=deltak;
           rtension=rt;
           pk=pk;
           p1k=p1k},{ltension=lt} = 
        match tc with a::b::_ -> a,b| _ -> assert false in
      begin
        match sfirst with
          | `Curve f ->
              let u0 = curl_ratio f rt lt in
              let v0 = -. psi1k *. u0 in
              let w0 = 0. in
              [u0,v0,w0]
          | `Given f -> 
              let v0 = reduce_angle  (f -. (atan2 deltak.x deltak.y)) in  
              let u0 = 0. in
              let w0 = 0. in
              [u0,v0,w0]
      end in
    let uvwkl,thetan = aux uvwkl tc in
    let thetal = calc_angle thetan [] uvwkl in
    let splines = calc_control_points smin [] tc thetal in
    splines



  (*     let data,uvwkl = 
         let lt,p1,n = tc in
         let cond,p0,rt = first in
         let pk = p0 in
         let p1k = p1 in
         let deltak = pk -/ p1k in
         let dk = P.norm deltak in
         let psi1k = match n with 
         | COpen(_,(_,p2k,_)) -> psi pk p1k p2k
         | _ -> assert false in
         let rtension = rt in
         let ltension = rtension in (* dumb value*)
         let data = [{dk=dk;psi1k=psi1k;deltak=deltak;
         ltension=ltension;
         rtension=rtension;
         pk=pk;
         p1k=p1k}] in
  *)

  (*   let cycle_or_not l = 
       let put_to_the_end 
  *)
  let create_temp_data rt lt pk1 pk p1k p2k =
    let deltak = p1k -/ pk in
    let dk = P.norm deltak in
    let deltak1 = pk -/ pk1 in
    let dk1 = P.norm deltak1 in
    let psi1k = psi pk p1k p2k in
    let psik = psi pk1 pk p1k in
    {psi1k = psi1k; psik = psik;
     dk = dk;dk1 = dk1;
     deltak = deltak;
     rtension = rt;
     ltension = lt;
     pk = pk;
     p1k = p1k
    }

  let print_data fmt p = 
    Format.fprintf fmt "{@[psi1k=%f;@.psik=%f;@.dk=%f;@.deltak=%a;@.rtension=%f;@.ltension=%f;@.pk=%a;@.p1k=%a@]}" p.psi1k p.psik p.dk P.print p.deltak p.rtension p.ltension P.print p.pk P.print p.p1k
      

  let splines_of acc l sfirst send =
    Format.printf "@.[@[%a@]]@." (fun fmt x ->
                                         List.iter (fun (pk1,(lt,rt),pk) ->
                                                      fprintf fmt "(%a,%f,%f,%a);" P.print pk1 lt rt P.print pk) x) l;
    let rec datas = function
      | []|[_] -> []
      | [(pk1,(_,rt),pk);(_,(lt,_),p1k)] -> [create_temp_data rt lt pk1 pk p1k p1k]
      | (pk1,(_,rt),pk)::((_,(lt,_),p1k)::(_,_,p2k)::_ as ln) ->
          create_temp_data rt lt pk1 pk p1k p2k::(datas ln) in
    let data = datas l in
    Format.printf "@.[@[%a@]]@." (fun fmt ->
                                         List.iter (fun e -> 
                             Format.fprintf fmt "%a;@." print_data e)) data;
    match data with
      | [] | [_] -> []
      | _ -> Format.printf "start...@?";
          let res = solve_choices_nocycle 1. (*mettre le bon*)
            sfirst send data in
         Format.printf "done@.";res
            
  let one_spline path p1 join p2 = 
    match join with
      | JLine -> (create_line p1 p2).pl@path
      | JControls (c1,c2) -> (create p1 c1 c2 p2).pl@path
      | JTension (d1,t1,t2,d2) -> (create_line p1 p2).pl@path (*TODO*)
      | JCurve (d1,d2) -> (create_line p1 p2).pl@path (*TODO*)
      | JCurveNoInflex (d1,d2) -> (create_line p1 p2).pl@path (*TODO*)

  let last_point_of = function
    | Start p1  | Cons (_,_,p1) -> p1
    | Start_Path p |Append_Path (_,_,p) -> last_point p

  let c_to_cl = function
    | DCurl x -> `Curve x
    | DVec {x=x;y=y} -> `Given (atan2 x y)
    | DNo -> `Curve 0.

  let c_to_cr (rt,lt) deltak1 = function
    | DCurl x -> `Curve (rt,lt,x)
    | DVec {x=x;y=y} -> `Given (deltak1,atan2 x y)
    | DNo -> `Curve (1.,1.,0.)

  let left_dir_of_join = function
     | JLine -> DCurl 0.
     | JControls (c1,c2) -> DCurl 0.
     | JTension (d,_,_,_) 
     | JCurve (d,_)
     | JCurveNoInflex (d,_) -> d

  let right_dir_of_join = function
     | JLine -> DCurl 0.
     | JControls (c1,c2) -> DCurl 0.
     | JTension (_,_,_,d) 
     | JCurve (_,d)
     | JCurveNoInflex (_,d) -> d

  let rec aux accl send = function
    | Start _ -> splines_of [] accl (`Curve 0.) send
    | Cons(c,
           (JTension (DNo,_,_,DNo)
           | JCurve (DNo,DNo)
           | JCurveNoInflex (DNo,DNo) as join),p2) ->
        let p1 = last_point_of c in
        (aux ((p1,tension_of join,p2)::accl) send c)
    | Cons(c,
           ( JTension (d,_,_,DNo)
           | JCurve (d,DNo)
           | JCurveNoInflex (d,DNo) as join),p2) -> 
        let p1 = last_point_of c in
        let sfirst = c_to_cl d in
        let nsend = c_to_cr (tension_of join) (P.sub p2 p1) d in
        (splines_of (aux [] nsend c) ((p1,tension_of join,p2)::accl) sfirst send)
    | Cons(c,
           (JTension (DNo,_,_,d)
           | JCurve (DNo,d)
           | JCurveNoInflex (DNo,d) as join),p2) -> 
        let p1 = last_point_of c in
        let sfirst = c_to_cl d in
        let nsend = c_to_cr (tension_of join) (P.sub p1 p2) d in
        (splines_of (aux [(p1,tension_of join,p2)] nsend c) accl sfirst send)
    | Cons(c,join,p2) ->
        let p1 = last_point_of c in
        let nsend = c_to_cr (tension_of join) (P.sub p1 p2) (right_dir_of_join join) in
        let sfirst = c_to_cl (left_dir_of_join join) in
        (splines_of (one_spline (aux [] nsend c) p1 join p2) accl sfirst  send)
    | Start_Path p -> List.rev p.pl
    | Append_Path (c,
                   ( JTension (DNo,_,_,d)
                   | JCurve (DNo,d)
                   | JCurveNoInflex (DNo,d) as join),p) -> 
        let p1 = last_point_of c in
        let p2 = (List.hd p.pl).sa in
        let sfirst = `Curve 0. in (*false*)
        let nsend = c_to_cr (tension_of join) (P.sub p1 p2) d in
        (splines_of (List.rev_append p.pl (aux [(p1,tension_of join,p2)] nsend c)) accl sfirst send)
    | Append_Path (c,
                   ( JTension (d,_,_,DNo)
                   | JCurve (d,DNo)
                   | JCurveNoInflex (d,DNo) as join),p) -> 
        let p1 = last_point_of c in
        let p2 = (List.hd p.pl).sa in
        let sfirst = `Curve 0. in (*false*)
        let nsend = c_to_cr (tension_of join) (P.sub p1 p2) d in
        (splines_of (List.rev_append p.pl (one_spline (aux [] nsend c) p1 join p2)) accl sfirst send)
    | Append_Path (c,join,p) -> 
        let p1 = last_point_of c in
        let p2 = (List.hd p.pl).sa in
        let nsend = c_to_cr (tension_of join) (P.sub p1 p2) (right_dir_of_join join) in
        let sfirst = `Curve 0. in
        (splines_of (List.rev_append p.pl (one_spline (aux [] nsend c) p1 join p2)) accl sfirst send)


  let true_to_path mp =
    { pl = aux [] (`Curve (1.,1.,0.)) mp; cycle = false}
*)    
       
 let kto_path ?cycle = function
   | Start p -> Point p
   | mp -> 
       let res = Path { pl = kmeta_to_path ?cycle mp; cycle = cycle <> None} in
       if info then
         Format.printf "@[end : @[%a@]@]@." printf res;
       res

  let rec to_path_simple = function
    | Start p -> create_line p p
    | Cons (pa,JLine,p) -> add_end_line (to_path_simple pa) p
    | Cons (pa,JControls(c1,c2),p) -> add_end_spline (to_path_simple pa) c1 c2 p
    | Start_Path p -> Path {pl=p;cycle=false}
    | Append_Path (p1,JControls(c1,c2),p2) -> append (to_path_simple p1) c1 c2 (Path {pl=p2;cycle=false})
    | (Cons(pa,JCurve _,p)) -> add_end_line (to_path_simple pa) p (* Faux*)
    |p -> Format.printf "not implemented %a@." print p; not_implemented "to_path"

  let knot p = p
    
  let vec_direction p = DVec p
  let curl_direction f = DCurl f
  let no_direction = DNo

  let start k = Start k
    
  let line_joint = JLine
  let curve_joint dir1 dir2 = JCurve(dir1,dir2)
  let curve_no_inflex_joint dir1 dir2 = JCurveNoInflex(dir1,dir2)
  let tension_joint dir1 f1 f2 dir2 = JTension (dir1,f1,f2,dir2)
  let controls_joint p1 p2 = JControls (p1,p2)
    

  let concat p j k = Cons (p,j,k)
  let rec append p j = function
    | Start knot -> Cons (p,j,knot)
    | Cons(p2,j2,k2) -> Cons(append p j p2,j2,k2)
    | Start_Path p2 -> Append_Path(p,j,p2)
    | Append_Path (p2,j2,p3) -> Append_Path(append p j p2,j2,p3)

  let to_path p = kto_path p
  let cycle j p = kto_path ~cycle:j p

  let from_path = function
    | Path p -> Start_Path p.pl
    | Point p -> Start p
end

module Approx =
struct
  let lineto = create_lines
  module M = Metapath
  let simple_join = M.curve_joint M.no_direction M.no_direction
  let curve l = 
    let rec aux = function
    | [] -> assert false
    | [a] -> M.start (M.knot a)
    | a::l -> M.concat (aux l) simple_join (M.knot a) in
    aux (List.rev l)
  let fullcircle_ l = 
    let l2 = l/.2. in 
    M.cycle simple_join (curve [{x=l2;y=0.};{x=0.;y=l2};{x= -.l2;y=0.};{x=0.;y= -.l2}])

  let fullcircle1 = lazy (fullcircle_ 1.)
  let fullcircle = function
    | 1. -> Lazy.force fullcircle1
    | l  -> fullcircle_ l

  let halfcirle l = subpath (fullcircle l) 0. 2. (* 2. because fullcircle is defined with 4 points *)
  let quartercircle l = subpath (fullcircle l) 0. 1.
  let unitsquare l = (close (create_lines [{x=0.;y=0.};{x=l;y=0.};{x=l;y=l};{x=0.;y=l};{x=0.;y=0.}]))
end

module ToCairo =
struct

  type pen = Matrix.t

  let draw_path cr = function
    |Path p ->
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
        if p.cycle then Cairo.close_path cr
    | Point _ -> failwith "Metapost fail in that case what should I do???"

  let stroke cr ?(pen=Matrix.identity) = function
    | Path _ as path -> 
        draw_path cr path;
        Cairo.save cr;
        Matrix.set cr pen;
        Cairo.stroke cr;
        Cairo.restore cr;
    | Point p ->
        Cairo.save cr;
        Matrix.transform cr (Matrix.translation p);
        Matrix.transform cr pen;
        draw_path cr (Approx.fullcircle 1.);
        Cairo.fill cr;
        Cairo.restore cr
        


    let fill cr path =
      draw_path cr path; Cairo.fill cr

end

module Epure =
struct
  (* A rendre plus performant ou pas*)
  type t = spline list list
  let print fmt p = 
    Format.fprintf fmt "@[[%a]" 
      (fun fmt -> List.iter (List.iter (fun e -> Format.fprintf fmt "%a;" print_spline e))) p
  let empty = []
  let create = function
    |Path p -> [p.pl]
    |Point p -> [match of_bounding_box (p,p) with Path p -> p.pl | Point _ -> assert false]
  let of_path = create
  let union x y = List.rev_append x y
  let transform t x = List.map (fun x -> transform_aux t x) x
  let bounding_box sl =
    let (x_min,y_min,x_max,y_max) = 
      list_min_max (list_min_max give_bound_precise) sl in
    ({x=x_min;y=y_min},{x=x_max;y=y_max})
  let of_bounding_box l = create (of_bounding_box l)

  let draw cr p = List.iter (List.iter 
                               (function 
                                    s -> 
                                      Cairo.move_to cr s.sa.x s.sa.y ;
                                      Cairo.curve_to cr 
                                        s.sb.x s.sb.y 
                                        s.sc.x s.sc.y 
                                        s.sd.x s.sd.y)) p;
    Cairo.stroke cr
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

