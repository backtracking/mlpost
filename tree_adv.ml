type 'a t = Node of 'a * 'a t list

let rec map f (Node (x,l)) = Node (f x, List.map (map f) l)

let rec map2 f (Node (x,l1)) (Node (y,l2)) =
  Node (f x y, List.map2 (map2 f) l1 l2)

let rec combine (Node (x,l1)) (Node (y,l2)) =
  Node ((x,y), List.map2 combine l1 l2)

let rec split (Node ((x,y),l)) = 
  let l1,l2 = List.split (List.map split l) in
  Node (x,l1), Node (y,l2)

let rec fold f acc (Node (x,l)) =
  let acc = List.fold_left (fold f) acc l in
  f acc x

let filter_option f t =
  let rec aux (Node (x,l)) =
    match f x with
      | None -> None
      | Some x ->
	  let l = List.map aux l in
	  let l = List.filter (function None -> false | Some _ -> true) l in
	  let l = List.map (function None -> assert false | Some x -> x) l in
	  Some (Node (x,l)) in
  match aux t with
    | None -> invalid_arg "Tree_adv.filter"
    | Some x -> x

let filter f t =
  filter_option (fun a -> if f a then None else Some a) t

let root_map f t =
  let rec aux r (Node (x,l)) = Node (f r x, List.map (aux (Some x)) l) in
  aux None t

let map_children f t =
  let rec aux (Node (x,l)) =
    let child = List.map (function Node (x,_) -> x) l in
    Node (f x child, List.map aux l) in
  aux t


let gen_place ~width ~height ~set_pos f t =
  let box_from_a z = Box.empty ~width:(width z) ~height:(height z) () in
  let box_tree = map box_from_a t in
  let b = f box_tree in
  map2 (fun z e -> set_pos (Box.ctr (Box.sub e b)) z) t box_tree

module Simple = struct

  let place ?(cs=Num.bp 5.) ?(ls=Num.bp 12.) ?(valign=`Center) ?(halign=`North)
     t =
      let rec aux (Node (x,l)) =
        let l = Box.hbox ~padding:cs ~pos:halign (List.map aux l) in 
        Box.vbox ~padding:ls ~pos:valign [x;l] in
      aux t

  let place ~width ~height ~set_pos ?ls ?cs ?valign ?halign t =
    gen_place ~width ~height ~set_pos (place ?ls ?cs ?valign ?halign) t

end

let place = Simple.place

open Command
let draw to_box t = 
  fold (fun acc x -> acc ++ Box.draw (to_box x)) Command.nop t

let gen_draw_arrows default ~style ~corner t = 
  root_map (fun a b -> 
    match a with 
    | None -> default
    | Some a -> style (corner `South a) (corner `North b)) t


let wrap_whs_box give_box mod_box f =
  let width a = Box.width (give_box a) in
  let height a = Box.height (give_box a) in
  let set_pos p a = mod_box a (Box.center p (give_box a)) in
  f ~width ~height ~set_pos

let wrap_corner_box give_box f =
  let corner p a = Box.corner p (give_box a) in
  f ~corner


module Overlays =
struct
    type interval = | Bet of int * int (** [|a,b|] *)
                    | Bef of int       (** ]|-oo,a|] *)
                    | Aft of int       (** [|a,+oo|[ *)
                    | Nev              (** emptyset *)
                    | Alw              (** N *)

    let in_interval i = function
      | Bet (x,y) when x<=i && i<=y -> true
      | Bef x when i<=x -> true
      | Aft x when x<=i -> true
      | Alw -> true
      | _ -> false

    let min_interval n = function
      | Bet (a,_) -> min a n
      | Bef a -> min a n
      | Aft a -> min a n
      | _ -> n

    let max_interval n = function
      | Bet (_,b) -> max b n
      | Bef b -> max b n
      | Aft b -> max b n
      | _ -> n

    let min_tree to_interval t =
      let f n a = min_interval n (to_interval a) in
      fold f max_int t

    let max_tree to_interval t =
      let f n a = max_interval n (to_interval a) in
      fold f min_int t

    type 'a spec = (interval * 'a) list

    let rec assoq n = function
      | [] -> raise Not_found
      | (i,a)::l when in_interval n i -> a
      | _::l -> assoq n l

    let max to_num = function
      | [] -> invalid_arg "Tree_adv.Overlays.width"
      | (_,a)::l ->
	  List.fold_left (fun w (_,p) -> Num.maxn w (to_num p)) (to_num a) l

    let set_pos sp pos =
	List.map (fun (i,b) -> i,sp pos b)

end