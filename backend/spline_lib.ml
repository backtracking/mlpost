open Format
exception Not_implemented of string

let not_implemented s = raise (Not_implemented s)

module Error = struct
  let max_absc f = 
    invalid_arg (f^": the abscissa given is greater than max_abscissa")
  let min_absc f =
    invalid_arg (f^": the abscissa given is smaller than min_abscissa")
  let absc_point f = invalid_arg (f^": a point has only the abscissa 0.")
  let dir_point f = invalid_arg (f^": a point has no direction.")
end
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

let pt_f fmt p = Format.fprintf fmt "{@[ %.20g,@ %.20g @]}" p.x p.y

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

let with_last f p acc = 
  let rec aux = function
    | [] -> assert false
    | [{sc=sc; sd = sd; smax = smax} as e] -> 
        e :: (f sc sd smax) :: acc
    | a::l -> a::(aux l) 
  in
  {p with pl = aux p.pl}

let add_end p c d =
  match p with
  | Point p -> create p c c d
  | Path p ->
      Path (with_last 
        (fun mb a smax -> create_with_offset smax a (2. */ a -/ mb) c d) p [])

let add_end_line p d =
  match p with
  | Point p -> create_line p d
  | Path p ->
      Path (with_last (fun mb a smax -> create_with_offset smax a a d d) p [])

let add_end_spline p sb sc d =
  match p with
  | Point p -> create p sb sc d
  | Path p ->
      Path (with_last (fun _ a smax -> create_with_offset smax a sb sc d) p [])

let f4 f a b c d = f (f a b) (f c d)

let cubic a b c d t =
  t*.(t*.(t*.(d +. 3.*.(b -. c) -. a) +. 3. *. (c -. (2. *. b) +. a)) 
      +. 3. *. (b -. a)) +. a
    (*  ((t^3)*(d - (3*c) + (3*b) - a)) + (3*(t^2)*(c - (2*b) + a)) + 
     *  (3*t*(b - a)) + a*)
    (*  d *. (t**3.) +. 3. *. c *. (t**2.) *. (1. -. t) +. 3. *. b *. (t**1.)
     *  *.(1. -. t)**2. +. a *. (1. -. t)**3.*)

let cubic_point s t =
  { x=cubic s.sa.x s.sb.x s.sc.x s.sd.x t;
    y=cubic s.sa.y s.sb.y s.sc.y s.sd.y t;}

let cubic_point_s s t = cubic_point s (_01_of_s s t)

let abscissa_to_point p0 t = 
  match p0 with
    | Path p ->
        let rec aux = function
          |[] -> Error.max_absc "abscissa_to_point"
          | a::l when a.smax >= t -> cubic_point_s a t
          | _::l -> aux l 
        in
        if min_abscissa p0 > t then Error.min_absc "abscissa_to_point"
        else aux p.pl
    | Point p when t = 0. -> p
    | Point _ -> Error.absc_point "abscissa_to_point"

let direction_of_abscissa_aux s t = 
  (* An expression as polynomial:
    short but lots of point operations
    (d-3*c+3*b-a)*t^2+(2*c-4*b+2*a)*t+b-a *) 
(*
  t */ (t */ (s.sd -/ 3. */ (s.sc +/ s.sb) -/ s.sa) +/ 
  2. */ (s.sc +/ s.sa -/ 2. */ s.sb)) +/ s.sb -/ s.sa
*)
(* This expression is longer, but has less operations on points: *)
  (t**2.) */ s.sd +/ (((2. *. t) -. (3. *. (t**2.)))) */ s.sc +/ 
  ((1. -. (4. *. t)+.(3. *. (t**2.)))) */ s.sb +/ (-.((1. -. t)**2.)) */ s.sa

let direction_of_abscissa p0 t = 
  match p0 with
    | Point _ -> Error.dir_point "direction_of_abscissa"
    | Path p ->  
        let rec aux = function
          |[] -> Error.max_absc "direction_of_abscissa"
          | a::_ when a.smax >= t -> direction_of_abscissa_aux a (_01_of_s a t)
          | _::l -> aux l 
        in
        if min_abscissa p0 > t then Error.min_absc "direction_of_abscissa"
        else aux p.pl

let extremum a b c d =
  let test s l = if s>=0. && s<=1. then s::l else l in
  let sol delta = (delta -. (2.*.b) +. a +. c)/.(a -. d +. (3.*.(c -. b))) in
  let delta = ((b*.b) -. (c*.(b +. a -. c)) +. (d*.(a -. b)))**0.5 in
  match compare delta 0. with
    | x when x<0 -> []
    | 0 -> test (sol 0.) []
    | _ -> test (sol delta) (test (sol (-.delta)) [])

let remarquable a b c d = 0.::1.::(extremum a b c d)

let apply_x f s = f s.sa.x s.sb.x s.sc.x s.sd.x
let apply_y f s = f s.sa.y s.sb.y s.sc.y s.sd.y
(** simple intersection *)
let give_bound s =
  let x_max = apply_x (f4 max) s in
  let y_max = apply_y (f4 max) s in
  let x_min = apply_x (f4 min) s in
  let y_min = apply_y (f4 min) s in
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
      let x_remarq = List.map (apply_x cubic s) (apply_x remarquable s) in
      let y_remarq = List.map (apply_y cubic s) (apply_y remarquable s) in
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

exception Found of float*float

let intersect_fold f acc a b =
  let rec aux acc a b t1 t2 dt = function
    | 0 ->
        if is_intersect a b then f (t1 + (dt/2), t2 + (dt/2)) acc
        else acc
    | n ->
        if is_intersect a b then
          let n = n - 1 and dt = dt / 2 in
          let a1,a2 = bisect a and b1,b2 = bisect b in
          let acc = aux acc a1 b1 t1 t2 dt n in
          let acc = aux acc a1 b2 t1 (t2+dt) dt n in
          let acc = aux acc a2 b1 (t1+dt) t2 dt n in
          let acc = aux acc a2 b2 (t1+dt) (t2+dt) dt n in
          acc
        else acc
  in
  let nmax = int_of_float (2.**(float_of_int (!inter_depth+1))) in
  aux acc a b 0 0 nmax !inter_depth

let one_intersection_aux a b =
  let nmax = 2.**(float_of_int (!inter_depth+1)) in
  let f_from_i s x = s_of_01 s ((float_of_int x)*.(1./.nmax)) in
  intersect_fold 
    (fun (x,y) () -> raise (Found (f_from_i a x,f_from_i b y))) 
    () a b

let one_intersection a b = 
  match a,b with
    | Path a,Path b ->
        (try
          one_to_one2 (fun () -> one_intersection_aux) () a.pl b.pl;
           if debug then
             (* debugging changes behaviour here - is this intended? *)
             (Format.printf "one_intersection : Not_found use 0.,0.@."; 0.,0.)
           else raise Not_found
        with Found (t1,t2) -> (t1,t2))
    | _ -> 
        if debug then 
          Format.printf "one_intersection : Not_found not two paths@.";
        raise Not_found

module UF = Unionfind

let intersection_aux acc a b =
  if a=b then [] else
    let rem_noise delta mdelta = function 
      | [] -> [] 
      | noisy ->
          let uf = UF.init noisy in
          let link sel msel = 
            let sorted = 
              List.fast_sort (fun x y -> compare (sel x) (sel y)) noisy in
            let rec pass bef = function
              |[] -> ()
              |e::l -> 
                  if sel e - sel bef <= delta then
                    if abs (msel e - msel bef) <=mdelta then begin
                      UF.union e bef uf; pass e l
                    end else begin pass e l; pass bef l end
                  else pass e l
            in
            pass (List.hd sorted) (List.tl sorted) 
          in
          link fst snd; link snd fst;
          UF.fold_classes (fun x acc -> x :: acc) [] uf
    in
    let nmax = 2.**(float_of_int (!inter_depth+1)) in
    let l = intersect_fold (fun x acc -> x::acc) [] a b in
    let l = rem_noise (2 * !inter_depth) (16 * !inter_depth) l in
    let f_from_i s x = s_of_01 s (x *. (1./.nmax)) in
    let res = List.rev_map (fun (x,y) -> (f_from_i a x,f_from_i b y)) l in
    if debug then
      Format.printf "@[%a@]@." (fun fmt -> List.iter (pt_f fmt))
        (List.map (fun (t1,t2) -> 
          (cubic_point a t1) -/ (cubic_point b t2)) res);
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
 
let append_conv ap bp = 
  let union_conv = union_conv ap bp in
  (fun x -> union_conv x +. 1.)

let ext_list = function
  | [] -> assert false
  | a::l -> a,l

let append ap0 sb sc bp0 = 
  match bp0 with
  | Path bp ->
      let conv = append_conv ap0 bp0 in
        let l = 
          List.map 
            (fun b -> {b with smin=(conv b.smin)+.1.;smax=(conv b.smax)+.1.})
            bp.pl
        in
        let fbpconv,bpconv = ext_list l in
        begin match ap0 with
        | Path ap ->
            let spl =
              with_last
                (fun _ sa smin -> create_with_offset smin sa sb sc fbpconv.sa)
                ap bpconv
           in Path {spl with cycle = false}
        | Point p1 ->
            Path { bp with 
                  pl = ( create_spline ~start:true p1 sb sc fbpconv.sa)::bp.pl }
        end
  | Point p2 ->
      match ap0 with
      | Point p1 -> create p1 sb sc p2
      | Path p -> add_end ap0 sc p2

let reverse x = 
    match x with
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
                    smin=conv smax; smax=conv smin}::acc) l in
      Path {p with pl = aux [] p.pl}
  | Point _ as p -> p

(*left ((t^3)*(d + (3*(b - c)) - a)) + 
 *     ((t^2)*(d - (3*b) + (2*a))) + (t*((2*c) - b - a)) + b *)
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
      (t0 *. t0) */ s.sc +/ (2. *. t0 *. _1t0) */ s.sb +/ (_1t0 *. _1t0) */ s.sa
    in
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
          |[] -> Error.max_absc "split"
          | a::l when a.smax > t -> split_aux a t l
          | a::l -> let (p1,p2) = aux l in (a::p1,p2) in
        if min_abscissa p0 > t then Error.min_absc "split"
        else 
          let (p1,p2) = aux p.pl in
          (cast_path_to_point (List.hd p.pl).sa (Path {pl=p1;cycle = false}),p2)
    | Point _ when t = 0. -> p0,p0
    | Point _ -> Error.absc_point "split"
      
let subpath p t1 t2 = fst (split (snd (split p t1)) t2)

let cut_before a b = 
  snd (split b (fst (one_intersection b a)))

let cut_after a b = 
  let b = reverse b in
  reverse (snd (split b (fst (one_intersection b a)))) 

let dicho_split x = assert false

let norm2 a b = a*.a +. b*.b
  
let is_possible (axmin,aymin,axmax,aymax) (bxmin,bymin,bxmax,bymax) =
  match axmin > bxmax, aymin > bymax, axmax < bxmin, aymax < bymin with
  | true , true , _    , _     -> norm2 (axmin -. bxmax) (aymin -. bymax)
  | _    , _    , true , true  -> norm2 (axmax -. bxmin) (aymax -. bymin)
  | true , _    , _    , true  -> norm2 (axmin -. bxmax) (aymax -. bymin)
  | _    , true , true , _     -> norm2 (axmax -. bxmin) (aymin -. bymax)
  | false, true , false, _     -> norm2 0. (aymin -. bymax)
  | false, _    , false, true  -> norm2 0. (aymax -. bymin)
  | true , false, _    , false -> norm2 (axmin -. bxmax) 0.
  | _    , false, true , false -> norm2 (axmax -. bxmin) 0.
  | false, false, false, false -> 0. 

let dist_min_point_aux {x=px;y=py} pmin s =
  let is_possible_at a = is_possible (give_bound a) (px,py,px,py) in
  let nmax = 2.**(float_of_int (!inter_depth+1)) in
  let rec aux a ((min,_) as pmin) t1 dt = function
    | 0 -> 
        let t1 = float_of_int (t1 + dt/2) /. nmax in
        let {x=apx;y=apy} = cubic_point s t1 in
        let dist = norm2 (apx -. px) (apy -. py) in
        if dist < min then (dist,s_of_01 s t1) else pmin
    | n -> 
        let n = n-1 in
        let dt = dt/2 in
        let (af,al) = bisect a in
        let dist_af = is_possible_at af in
        let dist_al = is_possible_at al in
        let doit ((min,_) as pmin) dist am t = 
          if dist < min then aux am pmin t dt n else pmin 
        in
        if dist_af<dist_al then
          let pmin = doit pmin dist_af af t1 in
          doit pmin dist_al al (t1+dt)
        else
          let pmin = doit pmin dist_al al (t1+dt) in
          doit pmin dist_af af t1 
  in
  aux s pmin 0 (int_of_float nmax) !inter_depth
    
let dist_min_point p point = 
  match p with
    | Path p ->
        let one = List.hd p.pl in
        let l = norm2 (one.sa.x -. point.x) (one.sa.y -. point.y),one.smin in
        snd (List.fold_left (dist_min_point_aux point) l p.pl)
    | Point p -> 0.

let dist_min_path_aux pmin s1 s2 =
  let is_possible_at a b = is_possible (give_bound a) (give_bound b) in
  let nmax = 2.**(float_of_int (!inter_depth+1)) in
  let rec aux a b ((min,_) as pmin) t1 t2 dt = function
    | 0 -> let t1 = float_of_int (t1 + dt/2) /. nmax in
      let t2 = float_of_int (t2 + dt/2) /. nmax in
      let ap = cubic_point s1 t1 in
      let bp = cubic_point s2 t2 in
      let dist = norm2 (ap.x -. bp.x) (ap.y -. bp.y) in
      if dist < min then (dist,(s_of_01 s1 t1,s_of_01 s2 t2)) else pmin
    | n -> let n = n-1 in
      let dt = dt/2 in
      let (af,al) = bisect a in
      let (bf,bl) = bisect b in
      let doit dist am bm t1 t2 ((min,_) as pmin) = 
        if dist < min then aux am bm pmin t1 t2 dt n else pmin 
      in
      let l = [af,bf,t1,t2; af,bl,t1,t2+dt; al,bf,t1+dt,t2;al,bl,t1+dt,t2+dt] in
      let l = List.map (fun (am,bm,t1,t2) -> let dist = is_possible_at am bm in
                        dist, doit dist am bm t1 t2) l in
      let l = List.fast_sort (fun (da,_) (db,_) -> compare da db) l in
      List.fold_left (fun pmin (_,doit) -> doit pmin) pmin l in
  aux s1 s2 pmin 0 0 (int_of_float nmax) !inter_depth

let dist_min_path p1 p2 = 
  match p1, p2 with
    | Path p1, Path p2 ->
        let one1 = (List.hd p1.pl) in 
        let one2 = (List.hd p2.pl) in 
        let n = 
          norm2 (one1.sa.x -. one2.sa.x) (one1.sa.y -.  one2.sa.y),
          (one1.smin,one2.smin) 
        in
        snd (one_to_one2 dist_min_path_aux n p1.pl p2.pl)
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
  | Point _ -> invalid_arg ("This path cannot be closed")

let of_bounding_box ({x=x_min;y=y_min},{x=x_max;y=y_max}) =
  let dl = {x=x_min;y=y_min} in
  let dr = {x=x_max;y=y_min} in
  let ul = {x=x_min;y=y_max} in
  let ur = {x=x_max;y=y_max} in
  close (create_lines [ul;ur;dr;dl;ul])

let length p = max_abscissa p -. min_abscissa p

module Epure =
struct
  (* A rendre plus performant ou pas*)
  (* le point correspond à un écart à prendre autour de la bounding box *)
  type t = (spline list * point) list
  let print fmt p = 
    Format.fprintf fmt "@[[%a]" 
      (fun fmt -> 
        List.iter 
          (List.iter 
            (fun (e,f) -> 
              Format.fprintf fmt "%a[%a];" print_spline e P.print f))) p
  let empty = []
  let create ?(ecart=P.zero) = function
    |Path p -> [p.pl,ecart]
    |Point p -> 
        let x = 
          match of_bounding_box (p,p) with 
          | Path p -> p.pl 
          | Point _ -> assert false
        in
        [x, ecart]

  let of_path = create
  let union x y = List.rev_append x y
  let transform t x = List.map (fun (x,f) -> transform_aux t x, f) x
  let bounding_box sl =
    let (x_min,y_min,x_max,y_max) = 
      list_min_max (fun (e,f) -> 
                      let (x_min,y_min,x_max,y_max)=
                        list_min_max give_bound_precise e in
                      let p1,p2 = ({x=x_min;y=y_min}-/f,{x=x_max;y=y_max}+/f) in
                      (p1.x,p1.y,p2.x,p2.y)) sl in
    ({x=x_min;y=y_min},{x=x_max;y=y_max})
  let of_bounding_box l = create (of_bounding_box l)

  let draw cr p = List.iter (fun (e,_) -> List.iter 
                               (fun s -> 
                                      Cairo.move_to cr s.sa.x s.sa.y ;
                                      Cairo.curve_to cr 
                                        s.sb.x s.sb.y 
                                        s.sc.x s.sc.y 
                                        s.sd.x s.sd.y) e) p;
                    Cairo.stroke cr
end

