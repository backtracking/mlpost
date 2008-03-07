open Command
open Helpers

type t = N of string * t list

let leaf s = N (s, [])
let node s l = N (s, l)
let bin s x y = N (s, [x; y])

let scale = Num.cm

let draw t =
  let point x y = Point.p (scale x, scale y) in
  (* tree -> float * (float -> float -> box * figure) *)
  let rec draw (N (s, l)) =
    let l = List.map draw l in
    let w = float (max 0 (List.length l - 1)) in
    w,
    fun x y -> 
      let b = Box.circle (point x y) (Picture.tex s) in
      let x = ref (x -. w /. 2. -. 1.) in
      b, 
      draw_box b :: 
	List.map 
	(fun (_,f) -> 
	   x := !x +. 1.; 
	   let b',fig = f !x (y-.1.) in
	   append (seq fig) (box_arrow b b')
	) l
  in
  let _,f = draw t in
  snd (f 0. 0.)


  
