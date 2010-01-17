(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Johannes Kanig, Stephane Lescuyer                       *)
(*  Jean-Christophe Filliatre, Romain Bardou and Francois Bobot           *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

open Format
exception Not_implemented of string

let not_implemented s = raise (Not_implemented s)

module Error = struct
  let max_absc f = 
    invalid_arg (f^": the abscissa given is greater than max_abscissa")
  let min_absc ?value f =
    let value = match value with | None -> "" 
      | Some f -> ": "^(string_of_float f) in
    invalid_arg (f^": the abscissa given is smaller than min_abscissa"^value)
  let absc_point f = invalid_arg (f^": a point has only the abscissa 0.")
  let dir_point f = invalid_arg (f^": a point has no direction.")
end
module P = Point_lib
type point = P.t

let id x = x
open Point_lib
open Point_lib.Infix

let rec one_to_one2 f acc a b =
  List.fold_left 
    (fun acc ea -> 
      List.fold_left (fun acc eb -> f acc ea eb) acc b) 
    acc a

let debug = Spline.debug
type spline = Spline.t
type abscissa = Spline.abscissa

type path_ = {pl : spline list;
             cycle : bool}

type path = | Point of point
            | Path of path_

let is_closed = function 
  | Point _ -> false
  | Path p -> p.cycle

let is_a_point = function
  | Point p -> Some p
  | Path _ -> None

let rec print_list sep prf fmt = function
  | [] -> ()
  | [x] -> prf fmt x
  | (x::xs) -> prf fmt x; sep fmt (); print_list sep prf fmt xs
let semicolon fmt () = Format.fprintf fmt ";@ "

let print_splines = print_list semicolon Spline.print

let print fmt = function 
  | Point p -> fprintf fmt "@[Point %a@]" P.print p
  | Path p -> fprintf fmt "@[cycle : %b; %a@]" p.cycle print_splines p.pl

let create_point p = Point p
let create a b c d = 
  Path {pl = [Spline.create a b c d ]; cycle = false}

let create_line a d = create a a d d 

let create_lines = function 
  | [] -> assert false
  | [a] -> Point a
  | l -> 
      let rec aux = function
        | [] |[_]-> []
        | a::(d::_ as l) -> 
            (Spline.create a a d d) :: aux l
      in
      match aux l with
      | [] -> assert false
      | a::l -> Path {pl=a::l;cycle=false}


let min_abscissa = function
  | Path p -> Spline.min (List.hd p.pl)
  | Point _ -> 0.

let max_abscissa = function
  | Path p ->
      let rec aux = function
        | [] -> assert false
        | [a] -> Spline.max a
        | a::l -> aux l in
      aux p.pl
  | Point _ -> 0.

let with_last f p acc = 
  let rec aux = function
    | [] -> assert false
    | [e] -> 
        let sd = Spline.right_point e and sc = Spline.right_control_point e in
        e :: (f sc sd (Spline.max e)) :: acc
    | a::l -> a::(aux l) 
  in
  {p with pl = aux p.pl}

let add_end p c d =
  match p with
  | Point p -> create p c c d
  | Path p ->
      Path (with_last (fun mb a smax -> 
        Spline.create_with_offset smax a (2. */ a -/ mb) c d) p [])

let add_end_line p d =
  match p with
  | Point p -> create_line p d
  | Path p ->
      Path (with_last (fun mb a smax -> 
        Spline.create_with_offset smax a a d d) p [])

let add_end_spline p sb sc d =
  match p with
  | Point p -> create p sb sc d
  | Path p ->
      Path (with_last (fun _ a smax -> 
        Spline.create_with_offset smax a sb sc d) p [])

let abscissa_to_point p0 t = 
  match p0 with
    | Path p ->
        let rec aux = function
          |[] -> Error.max_absc "abscissa_to_point"
          | a::l when Spline.max a >= t -> Spline.point_of_s a t
          | _::l -> aux l 
        in
        if min_abscissa p0 > t then Error.min_absc "abscissa_to_point"
        else aux p.pl
    | Point p when t = 0. -> p
    | Point _ -> Error.absc_point "abscissa_to_point"

let metapost_of_abscissa p0 t = 
  match p0 with
    | Path p ->
        let rec aux s = function
          |[] -> Error.max_absc "metapost_of_abscissa"
          | a::l when Spline.max a >= t -> s +.(Spline._01_of_s a t)
          | _::l -> aux (s+.1.) l 
        in
        if min_abscissa p0 > t then Error.min_absc "metapost_of_abscissa"
        else aux 0. p.pl
    | Point p when t = 0. -> 0.
    | Point _ -> Error.absc_point "metapost_of_abscissa"

let abscissa_of_metapost p0 t = 
  match p0 with
    | Path p ->
        let rec aux t = function
          |[] -> Error.max_absc "abscissa_of_metapost"
          | a::l when 1. >= t -> Spline.s_of_01 a t
          | _::l -> aux (t-.1.) l 
        in
        if 0. > t then Error.min_absc ~value:t "abscissa_of_metapost"
        else aux t p.pl
    | Point p when t = 0. -> 0.
    | Point _ -> Error.absc_point "abscissa_of_metapost"

let direction_of_abscissa p0 t = 
  match p0 with
    | Point _ -> Error.dir_point "direction_of_abscissa"
    | Path p ->  
        let rec aux = function
          |[] -> Error.max_absc "direction_of_abscissa"
          | a::_ when Spline.max a >= t -> Spline.direction a (Spline._01_of_s a t)
          | _::l -> aux l 
        in
        if min_abscissa p0 > t then Error.min_absc "direction_of_abscissa"
        else aux p.pl

let list_min_max f p = 
  List.fold_left (fun (x_min,y_min,x_max,y_max) s ->
                    let (sx_min,sy_min,sx_max,sy_max) = f s in
                    (min x_min sx_min,min y_min sy_min,
                     max x_max sx_max,max y_max sy_max))
    (infinity,infinity,neg_infinity,neg_infinity) p

let unprecise_bounding_box = function
  | Path s -> 
      let (x_min,y_min,x_max,y_max) = 
        list_min_max Spline.bounding_box s.pl in
      ({x=x_min;y=y_min},{x=x_max;y=y_max})
  | Point s -> s,s

let bounding_box = function
  | Path s ->
      let (x_min,y_min,x_max,y_max) = 
        list_min_max Spline.precise_bounding_box s.pl in
      ({x=x_min;y=y_min},{x=x_max;y=y_max})
  | Point s -> (s,s)

exception Found of (float * float)

let one_intersection a b = 
  match a,b with
    | Path a,Path b ->
        (try
          one_to_one2 (fun () a b -> 
            try raise (Found (Spline.one_intersection a b))
            with Not_found -> ()) () a.pl b.pl;
           if debug then Format.printf "one_intersection : Not_found@.";
           raise Not_found
        with Found a -> a)
    | _ -> 
        if debug then 
          Format.printf "one_intersection : Not_found not two paths@.";
        raise Not_found

let intersection a b = 
  match a,b with
    | Path a,Path b -> 
        one_to_one2 (fun acc a b -> acc@(Spline.intersection a b)) [] a.pl b.pl
    | _ -> []

let fold_left f acc = function
  | Path p -> List.fold_left (fun acc s -> Spline.apply4 (f acc) s) acc p.pl
  | Point _ -> acc
  
let iter f = function
  | Path p ->  List.iter (Spline.apply4 f) p.pl
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
      let conv x = append_conv ap0 bp0 x +. 1. in
        let l = List.map (fun b -> Spline.set_min_max conv conv b) bp.pl in
        let fbpconv,bpconv = ext_list l in
        begin match ap0 with
        | Path ap ->
            let spl =
              with_last (fun _ sa smin -> 
                Spline.create_with_offset smin sa sb sc 
                  (Spline.left_point fbpconv)) ap bpconv
           in Path {spl with cycle = false}
        | Point p1 ->
            Path {bp with pl = 
              (Spline.create p1 sb sc (Spline.left_point fbpconv)) ::bp.pl }
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
        | a::l -> aux (Spline.reverse conv a :: acc) l in
      Path {p with pl = aux [] p.pl}
  | Point _ as p -> p

(*left ((t^3)*(d + (3*(b - c)) - a)) + 
 *     ((t^2)*(d - (3*b) + (2*a))) + (t*((2*c) - b - a)) + b *)
(*right 3*d - c *)
let cast_path_to_point p = function
  | Path {pl=[];} -> Point p
  | x -> x
(*
(((t0*tt)^3)*(d + (3*(b - c)) - a)) + (3*((((t0*tt)^2)*(c + a - (2*b))) + (t0*tt*(b - a)))) + a
*)

let split_aux s t l = 
  match Spline.split s t with
  | Spline.Min -> [],Path {pl=s::l;cycle=false}
  | Spline.Max -> 
      let p =
        cast_path_to_point (Spline.right_point s) 
          (Path {pl=l;cycle=false}) in
      [s], p
  | Spline.InBetween (s1,s2) -> [s1], Path {pl = s2 :: l ; cycle = false }

let split p0 t = 
  match p0 with
    | Path p ->
        let rec aux = function
          |[] -> Error.max_absc "split"
          | a::l when Spline.max a > t -> split_aux a t l
          | a::l -> let (p1,p2) = aux l in (a::p1,p2) in
        if min_abscissa p0 > t then Error.min_absc "split"
        else 
          let (p1,p2) = aux p.pl in
          cast_path_to_point (Spline.left_point (List.hd p.pl)) 
            (Path {pl=p1;cycle = false}),p2
    | Point _ when t = 0. -> p0,p0
    | Point _ -> Error.absc_point "split"
      
let subpath p t1 t2 = fst (split (snd (split p t1)) t2)

let cut_before a b = 
  try 
    let t = (fst (one_intersection b a)) in
    let res = snd (split b t) in
(*    Format.printf "t : %f@.point %a@.b : %a@.res : %a@." t P.print (abscissa_to_point b t) print b print res;*)
    res
  with Not_found -> b

let cut_after a b = 
  try
    let b = reverse b in
    reverse (snd (split b (fst (one_intersection b a)))) 
  with Not_found -> b

let dicho_split x = assert false

let dist_min_point p point = 
  match p with
    | Path p ->
        let one = List.hd p.pl in
        let l = 
          Point_lib.norm2 (Spline.left_point one -/ point), 
          Spline.min one in
        snd (List.fold_left (Spline.dist_min_point point) l p.pl)
    | Point p -> 0.

let dist_min_path p1 p2 = 
  match p1, p2 with
    | Path p1, Path p2 ->
        let one1 = (List.hd p1.pl) in 
        let one2 = (List.hd p2.pl) in 
        let n = 
          Point_lib.norm2 (Spline.left_point one1 -/ Spline.left_point one2),
          (Spline.min one1, Spline.min one2) 
        in
        snd (one_to_one2 Spline.dist_min_path n p1.pl p2.pl)
    |Path _ as p1, Point p2 -> dist_min_point p1 p2,0.
    |Point p1, (Path _ as p2) -> 0.,dist_min_point p2 p1
    |Point _, Point _ -> 0.,0.

let translate t p = 
  match p with
    | Path p ->
        Path {p with pl=List.map (Spline.translate t) p.pl}
    | Point p -> Point (p +/ t)


let transform_aux t p = 
  List.map (Spline.transform t) p

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
let metapost_length = function
  | Point _ -> 0.
  | Path p -> float_of_int (List.length p.pl)

module Epure =
struct
  (* A rendre plus performant ou pas*)
  (* le point correspond à un écart à prendre autour de la bounding box *)
  type pen = path
  type t = (spline list * pen) list
  let print fmt p = 
    Format.fprintf fmt "@[[%a]" 
      (fun fmt -> 
        List.iter 
          (List.iter 
            (fun (e,f) -> 
              Format.fprintf fmt "%a[%a];" Spline.print e P.print f))) p
  let empty = []
  let create ?(ecart=Point P.zero) = function
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

  let transform t x = List.map (fun (x,f) -> transform_aux t x, transform (Matrix.remove_translation t) f) x
  let bounding_box sl =
    let (x_min,y_min,x_max,y_max) = 
      list_min_max (fun (e,f) -> 
                      let (x_min,y_min,x_max,y_max)=
                        list_min_max Spline.precise_bounding_box e in
                      let pen_min,pen_max = bounding_box f in
                      (*Format.printf "pen : %a,%a@." P.print pen_min P.print pen_max;*)
                      let p1,p2 = ({x=x_min;y=y_min}+/pen_min,{x=x_max;y=y_max}+/pen_max) in
                       (*Format.printf "p1,p2 : %a,%a@." P.print p1 P.print p2;*)
                      (p1.x,p1.y,p2.x,p2.y)) sl in
    ({x=x_min;y=y_min},{x=x_max;y=y_max})
  let of_bounding_box l = create (of_bounding_box l)

end

