open Metapost
open Command
open Picture
open Path
open Num
open Num.Infix
open Helpers

(* the data to plot are computed here *)

let x2 = 9
let y2 = 6
let bresenham_data =
  let a = Array.create (x2+1) 0 in
  let y = ref 0 in
  let e = ref (2 * y2 - x2) in
  for x = 0 to x2 do
    a.(x) <- !y;
    if !e < 0 then
      e := !e + 2 * y2
    else begin
      y := !y + 1;
      e := !e + 2 * (y2 - x2)
    end
  done;
  a

(* drawing *)

let pen = Pen.default ~tr:([Transform.scaled (bp 1.5)]) ()

let grid w h d f =
  let p i j = bp (float i *. d), bp (float j *. d) in
  [iter 0 (w-1) (fun i -> iter 0 (h-1) (f i));
   iter 0 w (fun i -> draw ~pen (pathn [p i 0; p i h]));
   iter 0 h (fun j -> draw ~pen (pathn [p 0 j; p w j]))]

let fig = 
  let d = 10. in
  let p i j = bp (float i *. d), bp (float j *. d) in
  let p2 i j = bp ((0.5 +. float i) *. d), bp ((0.5 +. float j) *. d) in
  let pic q i j = draw_pic (Picture.center (Point.pt (p2 i j)) q) in
  let cell i j = 
    if j = bresenham_data.(i) then
      let sq = Path.scale (bp d) unitsquare in
      let sq = shift (Point.pt (p i j)) sq in
      fill ~color:Color.red sq
    else
      seq [] 
  in
  [seq (grid (x2+1) (y2+1) d cell);
   pic (tex "0") 0 (-1);
   pic (tex "0") (-1) 0;
   pic (tex "$x_2$") x2 (-1);
   pic (tex "$y_2$") (-1) y2;
  ]
