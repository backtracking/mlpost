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

open Mlpost
open Num
open Command
open Format
open Helpers
open Point
open Path
module T = Transform

let (++) x y = pt (cm x, cm y)
let shift x y = transform [Transform.shifted (x ++ y)]

let d1 = 
  let a = Box.circle (0. ++ 0.) (Picture.tex "$\\sqrt2$") in
  let b = Box.rect (2. ++ 0.) (Picture.tex "$\\pi$") in
  let pen = Pen.default ~tr:[Transform.scaled (f 3.)] () in
  [ draw_box a;
    draw_box ~fill:Color.purple b;
    draw
      ~color:Color.red
      (Path.shift (1. ++ 1.) (Box.bpath a));
    draw_label_arrow ~color:Color.orange ~pen 
      ~pos:Pupright (Picture.tex "foo") (Box.west a) (Box.south_east b);
    box_arrow ~color:Color.blue a b;
  ]

open Tree

let () = Random.self_init ()

let rec random_tree = function
  | 1 -> 
      leaf "1"
  | 2 -> 
(*       node ~style:Rect ~fill:(Color.cmyk 1. 0.5 0.3 0.2) "2" [leaf "1"] *)
      node ~style:Rect ~fill:(Color.rgb 0.5 0.3 0.2) "2" [leaf "1"]
(*
  | n when Random.bool () -> 
      node (string_of_int n) [random_tree (n-1)]
*)
  | n -> 
      let k = 1 + Random.int (n - 2) in
      node (string_of_int n) [random_tree k; random_tree (n - 1 - k)]

let d2c, d2s, d2sq, d2hsq = 
  let ls = bp (-1.0) in
  let tree = random_tree 17 in
  [draw ~ls ~node_style:Circle ~arrow_style:Directed ~edge_style:Curve
       ~fill:Color.yellow ~stroke:Color.blue ~pen:(Pen.circle ()) tree],
  [draw ~ls ~node_style:Circle ~arrow_style:Directed ~edge_style:Straight
       ~fill:Color.yellow ~stroke:Color.blue ~pen:(Pen.circle ()) tree],
  [draw ~ls ~node_style:Circle ~arrow_style:Directed ~edge_style:Square
       ~fill:Color.yellow ~stroke:Color.blue ~pen:(Pen.circle ()) tree],
  [draw ~ls ~node_style:Circle ~arrow_style:Directed ~edge_style:HalfSquare
       ~fill:Color.yellow ~stroke:Color.blue ~pen:(Pen.circle ()) tree]

let proval =
  let f = 7. in
  let pen = Pen.square ~tr:[T.yscaled (bp 0.5); T.rotated 40.] () in
  let check = jointpath [-1.2,1.2; 0., -2. ; 2., 2. ; 5., 5.] [JLine ; JCurve; JCurve] in
    [ fill ~color:(Color.gray 0.2) (Path.scale (Num.bp f) fullcircle) ;
      label ~pos:Pleft (Picture.tex "Pr") (Point.p (f /. (-4.),0.)) ;
      label ~pos:Pright (Picture.tex "al") (Point.p (f /. 4.,0.)) ;
      Command.draw ~color:Color.green ~pen check;]

let cheno011 =
  let p = path ~cycle:JCurve [(0.,0.); (30.,40.); (40.,-20.); (10.,20.)] in
  let pen = Pen.circle ~tr:[T.scaled (f 1.5)] () in
  [Command.draw p;
   seq (List.map
	   (fun (pos, l, i) -> 
	     Command.dotlabel ~pos (Picture.tex l) (point i p))
	   [Pbot, "0", 0.;  Pupleft, "1", 1. ;
	    Plowleft, "2", 2. ;  Ptop, "3", 3. ; Pleft, "4", 4. ]);
   Command.draw ~pen (subpath 1.3 3.2 p)]

open Dash

let d3 =
  let p = pathp [cmp (0., 0.); cmp (5., 0.)] in
  let pat = pattern [On (bp 6.); Off (bp 12.); On (bp 6.)] in
  [Command.draw p ~dashed:pat]


let pic = Picture.make (seq cheno011)

let d4 = 
  [draw_pic pic;
   iter 1 5 
     (fun i -> 
	draw_pic (Picture.transform [T.rotated (10. *. float i)] pic))
  ]

let d5 = 
  let t1 = random_tree 5 in
  let pic1 = Picture.make (Tree.draw t1) in
  let b1 = Box.rect (cmp (0.,0.)) pic1 in
  let t2 = random_tree 6 in
  let pic2 = Picture.make (Tree.draw t2) in
  let b2 = Box.rect (cmp (4.,0.)) pic2 in
  [ draw_box b1;
    draw_box b2;
    box_arrow b1 b2 ]

let d7 =
  let pic = 
    Picture.transform [T.scaled (f 4.)] (Picture.tex "bound this!") in
  let pbox = pathp ~style:JLine ~cycle:JLine
    [Picture.ulcorner pic; Picture.urcorner pic; 
     Picture.lrcorner pic; Picture.llcorner pic] in
    [Command.draw_pic pic;
     Command.draw (Picture.bbox pic);
     Command.draw pbox;
     Command.dotlabel ~pos:Pleft (Picture.tex "ulcorner") (Picture.ulcorner pic);
     Command.dotlabel ~pos:Pleft (Picture.tex "llcorner") (Picture.llcorner pic);
     Command.dotlabel ~pos:Pright (Picture.tex "urcorner") (Picture.urcorner pic);
     Command.dotlabel ~pos:Pright (Picture.tex "lrcorner") (Picture.lrcorner pic);
    ]
    
open Tree.Pic

let tree1 () = Picture.make (Tree.draw (random_tree (1 + Random.int 5)))

let rec random_tree2 = function
  | 1 -> 
      leaf (tree1 ())
  | 2 -> 
      node (tree1 ()) [leaf (tree1 ())]
  | n -> 
      let k = 1 + Random.int (n - 2) in
      node (tree1 ()) [random_tree2 k; random_tree2 (n - 1 - k)]

let d6 =
  [Tree.draw ~scale:(Scale.cm 3.) ~cs:(mm 0.2) (random_tree2 10)]

let half pic = Picture.transform [Transform.scaled (f 0.5)] pic

let rec right_split n pic = 
  if n <= 0 then pic
  else
    let smaller = right_split (n-1) (half pic) in
      Picture.beside pic (Picture.below smaller smaller)

let d11 =
  let p1 = Picture.transform [Transform.rotated 90.] (Picture.tex "recursion") in
    [Command.draw_pic (right_split 5 p1)]

let rec sierpinski p n =
  if n = 0 then p else
    let sp = sierpinski p (n-1) in
    let p = half sp in
    let p1 = Picture.beside p p in
      Picture.below p p1

let d12 = 
  let p1 = Picture.tex "A" in
    [Command.draw_pic (sierpinski p1 8)]

(** plots *)
open Plot
let sk = mk_skeleton 20 14 (Num.bp 20.) (Num.bp 20.)

let d13 =  [ (draw_grid sk)]

let squaref x = x *. x
let f2 i = sqrt (float_of_int i)
let f3 i = squaref (float_of_int i)

let d14 =
  let hdash _ = Dash.scaled 0.5 Dash.withdots in
  let vdash _ = Dash.scaled 2. Dash.evenly in
  let hvpen i = 
    if i mod 5 = 0 then
      Pen.default ~tr:([Transform.scaled (f 2.5)]) ()
    else Pen.default () in
  let pen = Pen.default ~tr:[Transform.scaled (f 4.)] () in
     [draw_grid ~hdash ~vdash ~hpen:hvpen ~vpen:hvpen sk;
       draw_func ~pen f2 sk;
       draw_func ~pen f3 sk
      ]

let f1 i =
  let aux = function
  | 0 -> 1
  | 1 | 2 -> 2
  | 3 | 4 -> 3
  | 5  -> 4
  | 6 | 7 -> 5
  | 8 |9 -> 6
  | 10 -> 7
  | 11 | 12 -> 8
  | 13 | 14 -> 9
  | 15 -> 10
  | 16 | 17 -> 11
  | 18 | 19 -> 12
  | 20 -> 13
  | _ -> 0
  in
    float_of_int (aux i)

let f2 i =
  let aux = function
  | 0 | 1 | 2 -> 0
  | 3 -> 1
  | 4 -> 2
  | 5 | 6 | 7 -> 3
  | 8 -> 4
  | 9 -> 5
  | 10 | 11 | 12 -> 6
  | 13 -> 7 
  | 14 -> 8
  | 15 | 16 | 17 -> 9
  | 18 -> 10
  | 19 -> 11
  | 20 -> 12
  | _ -> 0
  in
    float_of_int (aux i)

let f3 i =
  float_of_int ((i+3)/5)

let flab i = (Picture.transform
		[Transform.scaled (f 1.7)]
		(Picture.tex (Printf.sprintf "$f_{\\omega_%d}$" i)),
	      Command.Ptop, 19)
	
let instants = 
  let pen = Pen.default ~tr:[Transform.scaled (f 2.5)] () in
  let base = 
    Command.draw ~pen (Path.path ~style:JLine [(0.,-65.); (280.,-65.)]) in
  let tick i = 
    let xi = float_of_int i *. 14. in
    let yi = if f1 i = f1 (i-1) then -60. else -45. in
    let p = Path.path ~style:JLine [(xi,-65.); (xi, yi)] in
      Command.draw ~pen p
  in
    Command.seq 
      [base; Command.iter 0 20 tick; 
       Command.label (Picture.transform [Transform.scaled (f 2.)]
			(Picture.tex "$\\omega_1$")) (pt (f (-20.), f (-55.)))]

let florence =
  let sk = mk_skeleton 20 14 (f 14.) (f 20.) in
  let pen = Pen.default ~tr:[Transform.scaled (f 4.)] () in
  let pen2 = Pen.default ~tr:[Transform.scaled (f 3.)] () in
  let dash _ = Dash.scaled 0.5 Dash.withdots in
  let dash2 = Dash.scaled 0.66 Dash.withdots in
  let dash3 = Dash.scaled 0.9 Dash.evenly in
  let vcaption, hcaption = 
    let tr = [Transform.scaled (f 1.5)] in
      Picture.transform tr (Picture.tex "\\textsf{Number of ones}"),
    Picture.transform tr (Picture.tex "\\textsf{Instants}") in
  let plot = draw_func ~drawing:Stepwise ~style:JLine in
  [ draw_grid ~hdash:dash ~vdash:dash sk;
    draw_axes ~closed:true ~hcaption ~vcaption sk;
    plot ~pen ~label:(flab 1) f1 sk;
    plot ~pen:pen2 ~dashed:dash2 ~label:(flab 2) f2 sk;
    plot ~pen ~dashed:dash3 ~label:(flab 3) f3 sk;
    instants
  ]

let d15 =
  let s = "010101011111" in
  let cmd = ref [] in
  let dist = 5. in
  let () =
    String.iter 
      (fun c -> 
        let p = Picture.tex (String.make 1 c) in
          cmd := (p :: !cmd)) s in
  let i = ref 0 in
    List.map
      (fun p -> 
         incr i; Command.label ~pos:Pleft p 
                  (Point.p (dist *. (float_of_int !i), 0.))) !cmd

let d16 = 
  let p1 = Point.p (0.,50.) in
  let l = Point.length p1 in
  let p2 = Point.pt (l,l) in
  let p3 = Point.p (50.,0.) in
    [ Command.draw (pathp ~style:JLine [p3;p2;p1])]

(* let d17 = Command.logo *)

let shapes1 =
  List.fold_left Picture.below
    (Picture.tex "Shapes 1 !")
    [Shapes.rectangle (f 10.) (f 20.);
     Shapes.rectangle ~stroke:Color.purple ~thickness:4. (f 35.) (f 15.);
     Shapes.rectangle 
       ~fill:Color.blue ~stroke:Color.orange ~thickness:4. (f 15.) (f 35.);
     Shapes.rounded_rect (f 55.) (f 25.) (f 10.) (f 10.);
     Shapes.rounded_rect ~stroke:Color.green (f 55.) (f 25.) (f 20.) (f 5.);
(*      Shapes.rounded_rect ~fill:Color.black ~stroke:Color.red  *)
(*        ~thickness:2. 70. 25. 12.5 12.5; *)
     Shapes.rounded_rect ~fill:Color.black ~stroke:Color.red 
       ~thickness:2. (f 70.) (f 25.) (f 14.) (f 14.);
    ]
  
let shapes2 =
  List.fold_left Picture.below
    (Picture.tex "Shapes 2 !")
    [
      Shapes.arc_ellipse (f 10.) (f 10.) 0. 1.7;
      Shapes.arc_ellipse ~stroke:Color.red (f 30.) (f 10.) 0. 1.7;
      Shapes.arc_ellipse ~stroke:Color.red ~close:true (f 30.) (f 10.) 0. 1.7;
      Shapes.arc_ellipse 
	~fill:Color.black ~stroke:Color.red (f 30.) (f 10.) 0. 1.7;
      Shapes.ellipse (f 10.) (f 10.);
      Shapes.ellipse ~stroke:Color.red (f 30.) (f 10.);
      Shapes.ellipse ~fill:Color.black ~stroke:Color.red (f 30.) (f 10.);
    ]

let yannick =
  let tt s = Picture.tex ("\\texttt{" ^ s ^ "}") in
  let node s = node ~style:Rect (tt s) in
  let leaf s = leaf ~style:Rect (tt s) in

  let tree =
    node "ComposerPage"
      [ leaf "MemSet";
    node "ComposerMessages"
      [ node "ComposerMsg"
          [
        leaf "StrCpy";
        leaf "DeclarerPanneRobustesse"
          ]
      ]
      ]
  in
  let scale d = Num.bp (d *. 70.) in
    [draw ~scale ~fill:Color.orange tree]

let stack =
  let bl = 
    Box.valign ~dx:Num.one ~dy:two
      [Picture.tex "$x$"; 
       Picture.tex "topo"; 
       Picture.tex "{\\Large I}"] 
  in
  let cbl = List.map2 
    (fun b n -> fill ~color:(Color.color n) (Picture.corner_bbox (Box.picture b))) 
    bl 
    ["BlanchedAlmond"; "DarkSlateGray"; "medium violet red"]
  in
  seq cbl :: List.map Command.draw_box bl

let row =
  let bl = 
    Box.halign ~dx:Num.one ~dy:two ~spacing:Num.two
      [Picture.tex "$x$"; 
       Picture.tex "topo"; 
       Picture.tex "{\\Large I}"] 
  in
  let cbl = List.map2 
    (fun b n -> fill ~color:(Color.color n) (Picture.corner_bbox (Box.picture b))) 
    bl 
    ["BlanchedAlmond"; "DarkSlateGray"; "medium violet red"]
  in
  seq cbl :: List.map Command.draw_box bl

let stackl =
  let bll = 
    Box.tabularl ~dx:Num.one ~dy:two
      [[Picture.tex "$x$"]; 
       [Picture.tex "topo"]; 
       [Picture.tex "{\\Large I}"]]
  in
  let cbl = List.map2
    (fun b n -> fill ~color:(Color.color n) (Picture.corner_bbox (Box.picture b)))
     (List.map List.hd bll)
    ["BlanchedAlmond"; "DarkSlateGray"; "medium violet red"]
  in
    seq cbl :: List.map Command.draw_box (List.flatten bll)

let rowl =
  let bll = 
    Box.tabularl ~dx:Num.one ~dy:two
      [[Picture.tex "$x$"; 
	Picture.tex "topo"; 
	Picture.tex "{\\Large I}"]]
  in
  let cbl = List.map2
    (fun b n -> fill ~color:(Color.color n) (Picture.corner_bbox (Box.picture b)))
    (List.hd bll)
    ["BlanchedAlmond"; "DarkSlateGray"; "medium violet red"]
  in
    seq cbl :: List.map Command.draw_box (List.flatten bll)

let array =
  let bll =
    Box.tabularl ~dx:Num.one ~dy:two
      [[Picture.tex "$x$"; Picture.tex "topo"; Picture.tex "{\\Large I}"];
       [Picture.tex "$\\lambda$"; Picture.tex "yippie"; Picture.tex "{\\Large O}"];
       [Picture.tex "{\\it Lets}"; Picture.tex "go"; Picture.tex "{\\Large PHILS}"]]
  in
  let cbl = 
    List.flatten 
      (List.map 
	 (List.map2
	    (fun n b -> fill ~color:(Color.color n) 
	       (Picture.corner_bbox (Box.picture b)))
	    ["BlanchedAlmond"; "DarkSlateGray"; "medium violet red"])
	 bll)
  in
    seq cbl :: List.map Command.draw_box (List.flatten bll)


(* my bresenham avec tabular *)
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

let mybresenham =
  let d = 10. in
(*   let pen = Pen.default ~tr:([Transform.scaled (f 1.5)]) () in *)
  let cell i j = 
    let sq = Path.scale (f d) unitsquare in
    let color =
      if j = bresenham_data.(i) then 
	Color.color "OrangeRed"
      else Color.white 
    in
      fill ~color sq
  in
  let bm = Box.tabulari x2 y2 (fun i j -> Picture.make (cell i j)) in
    Array.fold_left 
      (fun l m -> Array.fold_left (fun l b -> (Command.draw_box b)::l) l m)
      [] bm

let postest =
  let str_to_pos s = Pos.from_pic (Picture.tex s) in
  let pl1 = List.map str_to_pos ["toto"; "$2$"; "{\\Large I}"] in
  let pl2 = List.map str_to_pos [ "$4$";  "{\\Large $5$}"; "{\\Large I}"] in
  let pl3 = List.map str_to_pos ["$7$"; "$8$"; "{\\Large I}"] in 
  let bl = 
    List.flatten 
      (Pos.valign ~dy:Num.two
        [Pos.halign ~dx:Num.one pl1;
         Pos.halign ~dx:Num.one ~spacing:(Num.bp 50.) pl2;
         Pos.halign ~dx:Num.one pl3]
      ).Pos.v in
  let bl = Picture.spin 90. (List.hd bl) :: List.tl bl in
    List.map (Command.draw_pic) bl

let box_align = 
  let l = List.map Picture.tex ["toto"; "{\\Large I}"; "$1$"] in
    List.map (Command.draw_box) 
      (Box.halign_to_box ~dy:Num.two ~spacing:(Num.bp 10.) l)
    



let figs = [ postest; box_align; 
             stack; row; stackl; rowl; array; mybresenham; ]
(*
            [Command.draw_pic shapes1]; [Command.draw_pic shapes2];
            d16; d1; d15; florence; d14; d13; 
             d11; d7; d6; d5; d4; cheno011; proval; d3;  
                          d2sq; d2hsq; d2s; d2c; ] 

*)
let figs =
  let r = ref 0 in
  List.map (fun f -> incr r; !r, f) figs

let () =
  Metapost.generate_mp "test/tests.mp" figs;
  Misc.write_to_formatted_file "test/tests.tex"
    (fun fmt ->
      fprintf fmt "\\documentclass[a4paper]{article}@.";
      fprintf fmt "\\usepackage[]{graphicx}@.";
      fprintf fmt "@[<hov 2>\\begin{document}@.";
      List.iter
        (fun (i,_) ->
          fprintf fmt "@\n %i\\quad" i;
	  fprintf fmt "\\includegraphics[width=\\textwidth,height=\\textwidth,keepaspectratio]{tests.%d}" i;
          fprintf fmt "@\n \\vspace{3cm}@\n"
        ) figs;
      fprintf fmt "@]@\n\\end{document}@.")
