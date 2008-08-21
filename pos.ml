(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Johannes Kanig, Stephane Lescuyer                       *)
(*  and Jean-Christophe Filliatre                                         *)
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

module type POS =
sig
  (* POS.repr is the type of objects that we know how to position.
   * In most implementations of POS, this type will be visible.
   * However, in many cases (see ALIGN below) it is convenient to store
   * information about center, size etc along with the object. That is what
   * POS.t is for (consider types of POS.ctr, POS.height, POS.width). POS.t will
   * generally be abstract. To actually move the object, solely its structure is
   * needed (shift all subobjects by the same amount). *)

  type t
  type repr

  val ctr : t -> Point.t
  val height : t -> Num.t
  val width : t -> Num.t

  (** shift an object wrt to an offset *)
  val shift : Point.t -> repr -> repr

  (** center an object at a certain point, get back its contents *)
  val center : Point.t -> t -> repr

  (* get back the structure of the object *)
  val v : t -> repr

end


open Num.Infix
open Command

module type SEQ =
sig
  module P : POS
  type 'a seq
  include POS with type repr = P.repr seq
  val horizontal : 
    ?dx:Num.t -> ?pos:Command.position -> P.t seq -> t

  val vertical : 
    ?dy:Num.t -> ?pos:Command.position -> P.t seq -> t

  val tabular : 
    ?dx:Num.t -> ?dy:Num.t -> ?pos:Command.position -> P.t seq seq -> t seq

end

module List_ (P : POS) : SEQ with type 'a seq = 'a list and module P = P  =
struct
  type repr = P.repr list
  type 'a seq = 'a list
  type t = { v : P.repr list; center : Point.t; height : Num.t; width : Num.t}
  
  let height x = x.height
  let width x = x.width
  let ctr x = x.center
  let v x = x.v
  module P = P

  let shift pt t = List.map (P.shift pt) t

  let center pt x = shift (Point.sub pt (ctr x)) (v x)

  let horizontal ?(dx=Num.zero) ?(pos=Pcenter) pl =
    let hmax = Num.fold_max P.height Num.zero pl in
    let hmax_2 = hmax /./ 2. in
    let rec make_new acc x = function
      | [] -> List.rev acc, x -/ dx
      | p :: pl ->
          let wp,hp = P.width p, P.height p in
          let y = 
            match pos with
              | Pcenter -> hmax_2
              | Ptop -> hmax -/ hp /./ 2.
              | Pbot -> hp /./ 2.
              | _ -> failwith "alignment not supported"
          in
          let c = Point.pt (x +/ wp /./ 2., y) in 
          let b = P.center c p in
            make_new (b::acc) (x +/ wp +/ dx) pl
    in
    let l,x = make_new [] Num.zero pl in
    let mycenter = Point.pt (x /./ 2., hmax_2) in     
      { v = l; width = x; height = hmax; center = mycenter }

  let vertical ?(dy=Num.zero) ?(pos=Pcenter) pl =
    let wmax = Num.fold_max P.width Num.zero pl in
    let wmax_2 = wmax /./ 2. in
    let rec make_new acc y = function
      | [] -> List.rev acc, y +/ dy
      | p :: pl ->
          let wp,hp = P.width p, P.height p in
          let x = 
            match pos with
              | Pcenter -> wmax_2
              | Pright -> wmax -/ wp /./ 2.
              | Pleft ->  wp /./ 2.
              | _ -> failwith "alignment not supported"
          in
          let c = Point.pt (x, y -/ hp /./ 2.) in 
          let b = P.center c p in
            make_new (b::acc) (y -/ hp -/ dy) pl
    in
    let l,y = make_new [] Num.zero pl in
    let mycenter = Point.pt (wmax_2, y /./ 2.) in
    { v = l; width = wmax; height = y; center = mycenter }

  let tabular ?(dx=Num.zero) ?(dy=Num.zero) ?(pos=Pcenter) pll =
    let hmaxl = List.map (Num.fold_max P.height Num.zero) pll in
    let rec calc_wmax pll =
      match pll with
	| []::_ -> []
	| _ -> let cols, qll =
	    List.fold_left
	      (fun (col,rem) pl -> (List.hd pl :: col, List.tl pl :: rem))
	      ([],[]) pll in
	    (Num.fold_max P.width Num.zero cols)::(calc_wmax qll)
    in
    let wmaxl = calc_wmax pll in
    let rec make_rows acc wmaxl x y h_2 pl =
      match pl, wmaxl with
	| [], [] -> List.rev acc, x -/ dx
	| [], _ | _, [] -> raise (Invalid_argument "Lists have different sizes")
	| p::ql, wrow :: wl ->
(*             let wp,hp = P.width p, P.height p in *)
(* 	    let dx' = (dx +/ wrow -/ wp) // Num.two in *)
(* 	    let dy = h_2 -/ hp // Num.two in *)
	    let c = Point.pt (x +/ (dx +/ wrow) /./ 2., y -/ h_2) in
	    let b = P.center c p in
(*  	    let b = rect ~dx:dx' ~dy c p in *)
	      make_rows (b::acc) wl (x +/ wrow +/ dx) y h_2 ql
    in
    let rec make_array hmaxl y pll =
      match pll, hmaxl with
	| [], [] -> []
	| [], _ | _, [] -> raise (Invalid_argument "Lists have different sizes")
	| row :: qll, hrow :: hl ->
	    let brow, w =
	      make_rows [] wmaxl Num.zero y ((hrow +/ dy) /./ 2.) row
	    in
	    let mycenter = Point.pt (w /./ 2., hrow /./ 2.) in
	      {v = brow; width = w; height = hrow; center = mycenter}::
		(make_array hl (y -/ hrow -/ dy) qll)
    in
      make_array hmaxl Num.zero pll


end

module Array_ (P : POS) : SEQ with type 'a seq = 'a array and module P = P  =
struct
  type repr = P.repr array
  type 'a seq = 'a array
  type t = { v : P.repr array; center : Point.t; height : Num.t; width : Num.t}
  
  let height x = x.height
  let width x = x.width
  let ctr x = x.center
  let v x = x.v
  module P = P

  let shift pt t = Array.map (P.shift pt) t

  let center pt x = shift (Point.sub pt (ctr x)) (v x)

  module L = List_(P)

  let horizontal ?(dx=Num.zero) ?(pos=Pcenter) pa =
    let pl = L.horizontal ~dx ~pos (Array.to_list pa) in
    { v = Array.of_list (L.v pl); center = L.ctr pl;
      width = L.width pl; height = L.height pl }

  let vertical ?(dy=Num.zero) ?(pos=Pcenter) pa =
    let pl = L.vertical ~dy ~pos (Array.to_list pa) in
    { v = Array.of_list (L.v pl); center = L.ctr pl;
      width = L.width pl; height = L.height pl }

  let tabular ?(dx=Num.zero) ?(dy=Num.zero) ?(pos=Pcenter) paa =
    let pll = 
      L.tabular ~dx ~dy ~pos (List.map Array.to_list (Array.to_list paa)) in
      Array.of_list 
	(List.map 
	   (fun pl -> { v = Array.of_list (L.v pl); center = L.ctr pl;
			width = L.width pl; height = L.height pl }) pll)
end

type 'a tree = N of 'a * 'a tree list

module type TREE =
sig
  module P : POS
  include POS with type repr = P.repr tree
  val place : ?dx:Num.t -> ?dy:Num.t -> P.t tree -> t
end

module Tree (P : POS) : TREE with module P = P =
struct
  module P = P
  module Aux =
  struct
    type repr = P.repr tree
    type repr' = repr
    type t = {ctr : Point.t; w : Num.t ; h : Num.t ; v : repr}
    type t' = t

    let v x = x.v
    let ctr x = x.ctr
    let height x = x.h
    let width x = x.w

    let rec shift pt (N (a,l)) = 
      N (P.shift pt a, List.map (shift pt) l)

    let center pt x = shift (Point.sub pt (ctr x)) (v x)
  end

  include Aux

  module TA = List_ (Aux)

  let rec place ?dx ?(dy=Num.zero) (N (a,l)) = 
    let pl = TA.horizontal ?dx ~pos:Ptop (List.map (place ?dx ~dy) l) in
    let w = Num.maxn (TA.width pl) (P.width a) in
    let h = TA.height pl +/ dy +/ P.height a in
    let ctr = Point.pt (0.5 *./ w , 0.5 *./ h) in 
    let new_point = Point.pt (0.5 *./ w, h -/ (0.5 *./ P.height a)) in
      { v = N (P.center new_point a, TA.v pl);
        w = w; h = h; ctr = ctr; }
end
