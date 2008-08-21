open Command
open Picture
open Path
open Num

let pen = Pen.default ~tr:([Transform.scaled (bp 1.5)]) ()

let grid w h d f =
  let p i j = bp (float i *. d), bp (float j *. d) in
  [iter 0 (w-1) (fun i -> iter 0 (h-1) (f i));
   iter 0 w (fun i -> draw ~pen (pathn [p i 0; p i h]));
   iter 0 h (fun j -> draw ~pen (pathn [p 0 j; p w j]))]

let bq = tex "\\font\\Chess=chess10 {\\Chess q}"
let question = tex "?"

let fig = 
  let d = 15. in
  let p i j = bp (float i *. d), bp (float j *. d) in
  let p2 i j = bp ((0.5 +. float i) *. d), bp ((0.5 +. float j) *. d) in
  let pic q i j = draw_pic (Picture.center (Point.pt (p2 i j)) q) in
  let cell i j = 
    let l = 
      if (i+j) mod 2 = 1 then 
	let sq = Path.scale (bp d) unitsquare in
	let sq = shift (Point.pt (p i j)) sq in
	[fill ~color:(Color.gray 0.7) sq] 
      else 
	[] 
    in
    seq 
      (if i = 4 && j = 7 || i = 3 && j = 5 || i = 6 && j = 6 then 
	 l @ [pic bq i j]
       else if j = 4 then 
	 l @ [pic question i j]
       else
	 l)
  in
  grid 8 8 d cell
