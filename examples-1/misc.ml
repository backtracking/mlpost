open Command
open Picture
open Path
open Num
open Num.Infix
open Helpers

(*parse <<togglescript>> *)

(*parse <<bresenham *)
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

let bresenham = 
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

(*parse >> *)

open Diag

(*parse <<cfg *)
let cfg =
  let pen = Pen.circle () in
  let empty_node x y = node x y "\\phantom{M}" in
  let a = empty_node 0. 4. in
  let b = empty_node 0. 3. in
  let inv = node 0. 2. "inv" in
  let c = empty_node 0. 1. in
  let d = empty_node 0. 0. in
  let do_ = node (-2.) 2. "do" in
  let diag = create [a;b;c;d;inv;do_] in
  let arrow = arrow diag in
  arrow a b ~lab:"$i\\leftarrow0$" ~pos:`Right;
  arrow b inv ~lab:"$m\\leftarrow t[i]$" ~pos:`Right;
  arrow c d ~lab:"$i\\ge n$" ~pos:`Right;
  arrow c do_ ~outd:Left ~ind:Down ~lab:"$i<n$" ~pos:`Lowleft;
  arrow inv c ~lab:"$i\\leftarrow i+1$" ~pos:`Right;
  arrow do_ inv ~lab:"$m\\ge t[i]$" ~pos:`Top;
  arrow do_ b ~outd:Up ~ind:Left ~lab:"$m<t[i]$" ~pos:`Upleft;
  [draw ~fill:Color.yellow ~stroke:Color.blue ~pen diag]
(*parse >> *)

let _ = 
  List.iter (fun (name,fig) -> Metapost.emit name fig)
  [ "bresenham", bresenham;
    "cfg", cfg;
  ]
