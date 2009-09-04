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

let root_map f t = 
  let rec aux r (Node (x,l)) = Node (f r x, List.map (aux (Some x)) l) in
  aux None t


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
