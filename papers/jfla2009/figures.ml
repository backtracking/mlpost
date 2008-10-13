
(* figures pour l'article JFLA *)

open Mlpost
open Command
open Picture
open Path
open Helpers
open Num
open Num.Infix
open Point
open Box

(* Stéphane *)
(** the old ugly version *)
(* let graph_sqrt = *)
(*   let u = Num.cm in *)
(*   let pen = Pen.circle ~tr:[Transform.scaled one] () in  *)
(*   let rec pg = function *)
(*     | 0 -> start (knot ~r:(vec up) ~scale:u (0.,0.)) *)
(*     | n -> let f = (float_of_int n /. 2.) in  *)
(* 	concat ~style:jCurve (pg (n-1)) (knot ~scale:u (f, sqrt f))  *)
(*   in *)
(*   [draw (pathn ~style:jLine [(zero,u 2.); (zero,zero); (u 4.,zero)]); *)
(*    draw ~pen (pg 8); *)
(*    label ~pos:`Lowright (tex "$ \\sqrt x$") (pt (u 3., u (sqrt 3.))); *)
(*    label ~pos:`Bot (tex "$x$") (pt (u 2., zero)); *)
(*    label ~pos:`Lowleft (tex "$y$") (pt (zero, u 1.))] *)

(** the new short one :) *)
let graph_sqrt =
  let u = cm 1. in
  let sk = Plot.mk_skeleton 4 3 u u in
  let label = Picture.tex "$y=\\sqrt{x+\\frac{1}{2}}$", `Upleft, 3 in
  let graph = Plot.draw_func ~label (fun x -> sqrt (float x +. 0.5)) sk in
  [graph; Plot.draw_simple_axes "$x$" "$y$" sk]

let architecture =
  let mk_box fill name m = 
    let m = "{\\tt " ^ m ^ "}" in
    Box.tex ~stroke:(Some Color.black) 
      ~style:RoundRect ~dx:(bp 5.) ~dy:(bp 5.) ~name ~fill m in
  let mk_unbox name m = 
    Box.tex ~style:RoundRect ~stroke:None ~dx:(bp 5.) ~dy:(bp 5.) ~name m in
    (* les types de base *)
  let fill = Color.color "salmon" in 
  let num = mk_box fill "num" "Num" in
  let point = mk_box fill "point" "Point" in
  let path = mk_box fill "path" "Path" in
  let dots = mk_unbox "dots" "$\\ldots$" in
  let cmd = mk_box fill "cmd" "Command" in
  let basictypes = Box.hbox ~padding:(mm 2.) [num; point; path; dots; cmd] in
    (* compile *)
  let compile = mk_unbox "compile" "\\tt Compile" in
  let compile_ext = 
    let dx = (Box.width basictypes -/ Box.width compile) /./ 2. in
    Box.hbox ~style:RoundRect ~dx ~fill ~stroke:(Some Color.black) [compile] in
    (* metapost *)
  let metapost = mk_unbox "metapost" "\\metapost" in
  let metapost_ext =
    let dx = (Box.width basictypes -/ Box.width metapost) /./ 2. in
      Box.hbox ~name:"mpost_ext" 
	~style:Rect ~dx ~stroke:(Some Color.black) [metapost] in
    (* composants avancés *)
  let fill = Color.color "pink" in 
  let box_ = mk_box fill "box" "\\phantom{p}Box\\phantom{p}" in
  let shapes = mk_box fill "shapes" "\\phantom{p}Shapes\\phantom{p}" in
  let arrows = mk_box fill "arrow" "\\phantom{p}Arrow\\phantom{p}" in
  let advanced = Box.hbox ~pos:`Bot ~padding:(mm 2.) [box_; shapes; arrows] in
    (* extensions *)
  let fill = Color.color "blanched almond" in 
  let tree = mk_box fill "tree" "\\phantom{g}Tree\\phantom{g}" in
  let diag = mk_box fill "diag" "\\phantom{g}Diag\\phantom{g}" in
  let plot = mk_box fill "plot" "\\phantom{g}Plot\\phantom{g}" in
  let extensions = Box.hbox ~pos:`Bot ~padding:(mm 2.) [tree; diag; plot] in
    (* wrapping *)
  let pyramid = 
    let pen = Pen.scale (Num.bp 1.0) (Pen.square ()) in
    Box.vbox ~padding:(mm 2.) ~pen
      ~dx:(bp 5.) ~dy:(bp 5.) ~style:RoundRect ~stroke:(Some Color.black)
      [extensions; advanced; basictypes; compile_ext; metapost_ext] in
  let mlpost = mk_unbox "mlpost" "\\tt Mlpost" in
  let mlpost_ext = 
    let dx = (Box.width pyramid -/ Box.width mlpost) /./ 2. in 
      Box.hbox ~dx [mlpost] in
  let full = Box.vbox ~padding:(mm (-1.)) [mlpost_ext; pyramid] in
  let _ = Box.set_stroke Color.black (Box.nth 1 full) in
    (* arrows *)
  let arrows = 
    let mp = Box.get "mpost_ext" full in
      List.map 
	(fun n -> Helpers.box_label_arrow 
	   ~outd:(Path.vec Point.down) ~pos:`Bot (Picture.make Command.nop)
	   (Box.get n full) mp)
	(* une variante *)
(*     let mp = Box.get "metapost" full in *)
(*       List.map  *)
(* 	(fun n -> Helpers.box_arrow  *)
(* 	   (Box.get n full) mp) *)
	["num"; "point"; "path"; "dots"; "cmd"]
  in
    arrows@[Box.draw full]

(* Romain *)
module Arrow = ExtArrow

open Num
let state = Box.tex ~style:Circle ~stroke:(Some Color.black)
let final = Box.box ~style:Circle 

let transition states tex pos ?outd ?ind x_name y_name = 
  let x = Box.get x_name states and y = Box.get y_name states in
  let outd = match outd with None -> None | Some a -> Some (vec (dir a)) in
  let ind = match ind with None -> None | Some a -> Some (vec (dir a)) in
  Arrow.draw ~tex ~pos (cpath ?outd ?ind x y) 

(*let loop box =
  let c = Box.ctr box in
  let a = Point.shift c (pt (cm 0., cm (-0.8))) in
  let p = Path.pathk [
    knotp ~r: (vec (dir 225.)) c;
    knotp a;
    knotp ~l: (vec (dir 135.)) c;
  ] in
  let bp = Box.bpath box in
  cut_after bp (cut_before bp p)*)

(*
let loop states tex pos name =
  let box = Box.get name states in
  let fdir, angle, x, y = match pos with
    | `Top -> Box.north, 0., 0., 0.4
    | `Left -> Box.west, 90., (-0.4), 0.
    | `Bot -> Box.south, 180., 0., (-0.4)
    | `Right -> Box.north, 270., 0.4, 0.
  in
  let a = Point.shift (fdir box) (Point.pt (cm x, cm y)) in
  let c = Box.ctr box in
  let p = Path.pathk [
    knotp ~r: (vec (dir (angle +. 45.))) c;
    knotp a;
    knotp ~l: (vec (dir (angle -. 45.))) c;
  ] in
  let bp = Box.bpath box in
  Arrow.draw ~tex ~pos: (pos :> Command.position)
    (cut_after bp (cut_before bp p))
*)
let loop states tex name =
  let box = Box.get name states in
  let a = Point.shift (Box.south box) (Point.pt (cm 0., cm (-0.4))) in
  let c = Box.ctr box in
  let p = Path.pathk [
    knotp ~r: (vec (dir 225.)) c;
    knotp a;
    knotp ~l: (vec (dir 135.)) c;
  ] in
  let bp = Box.bpath box in
  Arrow.draw ~tex ~pos:`Bot (cut_after bp (cut_before bp p))

let arrow_loop_explain_kind =
  Arrow.add_belt ~point: 0.9
    (Arrow.add_line ~dashed: Dash.evenly ~to_point: 0.1
       (Arrow.add_line ~dashed: Dash.evenly ~from_point: 0.9
          (Arrow.add_line ~from_point: 0.1 ~to_point: 0.9
             Arrow.empty)))

let loop_explain =
  let construct_pattern = Dash.pattern [Dash.on (bp 0.2); Dash.off (bp 1.)] in
  let arc_arrow =
    Arrow.add_head ~head: (Arrow.head_classic ~dashed: construct_pattern)
      (Arrow.add_line ~dashed: construct_pattern
         Arrow.empty)
  in
  let s = state "~~~~~~~~~~~" in
  let pt x y = Point.pt (cm x, cm y) in
  let p x y = Point.shift (Box.ctr s) (pt x y) in
  let a_pos = p 0. (-2.) in
  let angle = 180. in
  let c = Box.ctr s in
  let arrow_path = Path.pathk [
    knotp ~r: (vec (dir (angle +. 45.))) c;
    knotp a_pos;
    knotp ~l: (vec (dir (angle -. 45.))) c;
  ] in
  let vert = Path.pathk [
    knotp (p 0. 2.);
    knotp (p 0. (-3.));
  ] in
  let len = 1.2 in
  let diag1 = Path.pathk [
    knotp (p len len);
    knotp (p (-.len) (-.len));
  ] in
  let diag2 = Path.pathk [
    knotp (p len (-.len));
    knotp (p (-.len) len);
  ] in
  let construct = Command.draw ~dashed: construct_pattern in
  let circle = Path.shift (pt 0.64 0.) (Path.scale (cm 2.) Path.fullcircle) in
  let arc = cut_before vert (cut_after diag2 (cut_after vert circle)) in
  [
    Box.draw s;
    Command.dotlabel ~pos: `Lowleft (Picture.tex "$A$") a_pos;
    Arrow.draw ~kind: arrow_loop_explain_kind arrow_path;
    construct vert;
    construct diag1;
    construct diag2;
    Arrow.draw ~tex: "$45°$" ~pos: `Upleft ~kind: arc_arrow arc;
  ]

(*
let initial states pos name =
  let x = Box.get name states in
  let p = match pos with
    | `Left -> Box.west x
    | `Right -> Box.east x
    | `Top -> Box.north x
    | `Bot -> Box.south x
  in
  Arrow.draw (Path.pathp [ Point.shift p (Point.pt (cm (-0.3), zero)); p ])
*)
let initial states name =
  let b = Box.get name states in
  let p = Box.west b in
  Arrow.draw (Path.pathp [ Point.shift p (Point.pt (cm (-0.3), zero)); p ])

let automate_1 =
  let states = Box.vbox ~padding:(cm 0.8)
    [ Box.hbox ~padding:(cm 1.4)
      [ state ~name:"alpha" "$\\alpha$";
        state "$\\beta$" ];
      final (state "$\\gamma$") ]
  in
  [ Box.draw states ]

let automate =
  let states = Box.vbox ~padding: (cm 0.8)
    [ Box.hbox ~padding: (cm 1.4)
        [ state ~name: "alpha" "$\\alpha$";
          state ~name: "beta" "$\\beta$" ];
      final ~name: "gamma" (state "$\\gamma$") ]
  in
  [ Box.draw states;
    transition states "a" `Lowleft "alpha" "gamma";
    transition states "b" `Lowright "gamma" "beta";
    transition states "c" `Top ~outd: 25. ~ind: 335. "alpha" "beta";
    transition states "d" `Bot ~outd: 205. ~ind: 155. "beta" "alpha";
    loop states "e" "gamma";
    initial states "alpha" ]

let arrow_metapost =
  [Helpers.draw_simple_arrow ~outd:(vec (dir 90.)) ~ind:(vec (dir 90.))
     (Point.pt (cm 0., cm 0.5)) (Point.pt (cm 2., cm 0.5));
   Helpers.draw_simple_arrow
     (Point.pt (cm 4., cm 0.5)) (Point.pt (cm 6., cm 0.5));
   Helpers.draw_simple_arrow ~dashed:Dash.evenly ~outd:(vec (dir 90.))
     (Point.pt (cm 8., cm 0.)) (Point.pt (cm 10., cm 0.));
   Helpers.draw_simple_arrow
     ~pen:(Pen.scale (bp 2.5) (Pen.square ()))
     (Point.pt (cm 12., cm 0.)) (Point.pt (cm 14., cm 1.))]

let arrow_demo_path = Path.pathp [
  Point.pt (zero, zero);
  (Point.pt (cm 2., zero));
]
let arrow_simple =
  [Arrow.draw ~kind: Arrow.triangle_full arrow_demo_path]

let arrow_loop_explain =
  let pt x y = Point.pt (cm x, cm y) in
  let draw2 = Arrow.draw2 ~kind: arrow_loop_explain_kind in
  [
    draw2 ~outd:(vec (dir 45.)) ~ind: (vec (dir 45.)) (pt 0. 0.25) (pt 3. 0.25);
    draw2 (pt 5. 0.25) (pt 8. 0.25);
    draw2 ~outd:(vec (dir 45.)) (pt 10. 0.) (pt 13. 0.);
  ]

(* Johannes *)
open Box
let uml_client, uml = 
  let classblock name attr_list method_list = 
    let vbox = Box.vbox ~pos:`Left in
    Box.vblock ~pos:`Left ~name
      [ tex ("{\\bf " ^ name ^ "}");
        vbox (List.map tex attr_list); vbox (List.map tex method_list) ]
  in
  let a = classblock "BankAccount" 
            [ "balance : Dollars = $0$"] 
            ["deposit (amount : Dollars)"; "withdraw (amount : Dollars)" ] 
  in
  let b = classblock "Client" ["name : String"; "address : String" ] [] in
  let diag = Box.vbox ~padding:(cm 1.) [a;b] in
  [Box.draw b],
  [ Box.draw diag; 
    box_label_arrow ~pos:`Left (Picture.tex "owns") 
      (get "Client" diag) (get "BankAccount" diag) ]

open Tree

let sharing = 
  let tex s = tex ~name:s ~style:Circle ~dx:two ~dy:two s in
  let tree = 
    bin (tex "a")
      (bin (tex "b")
        (leaf (tex "c"))
        (bin (tex "d") 
          (leaf (tex "e")) 
          (leaf (tex "f")) 
        )
      )
      (node (tex "g") [leaf (tex "h")])
  in
  [ draw tree;
    box_arrow (get "h" tree) (get "f" tree);
    box_arrow (get "g" tree) (get "d" tree);
      ]

let stages = 
  let dx = bp 5. and dy = bp 5. in
  let tex' = tex ~style:RoundRect ~dx ~dy in
  let tex = tex' ~stroke:(Some Color.black) in
  let box name = box ~stroke:None ~dx:(mm 2.) ~name in
  let fml = box "fml" (tex "figure.ml") in
  let fmp = box "fmp" (tex "figure.mp") in
  let ps = 
    box "ps" (vbox ~stroke:(Some Color.black) ~style:RoundRect ~dx ~dy
             [tex' "figure.1"; tex' "(\\postscript)"]) in
  let arrowpic = pic ~stroke:None
       (Picture.make (draw_arrow (Path.path ~scale:cm [0.,0.; 0.,-1.]))) 
  in
  let all = hbox ~padding:(cm 3.5) [fml; fmp; ps ] in
  [ draw all;
    box_labelbox_arrow ~pos:`Top 
      (vbox [tex "\\ocaml"; arrowpic; tex' "compiler \\& ex\\'ecuter"])
      (get "fml" all) (get "fmp" all);
    box_labelbox_arrow ~pos:`Top 
      (vbox [tex "\\metapost"; arrowpic; tex' "interpr\\'eter" ] )
      (get "fmp" all) (get "ps" all);
  ]

let simple = 
  [ draw (tex "\\LaTeX");
    draw (shift (Point.pt (cm 1., zero)) (circle (empty ()))) ]

let align = 
  [ draw
    (hbox ~padding:(cm 1.) [tex "\\LaTeX"; circle (empty ())]) ]

let persistance = 
  let b = hbox ~padding:(cm 1.) [tex "\\LaTeX"; circle (empty ())] in
  [draw (vbox [b; set_stroke Color.black b; b])]


(* JC *)
let bresenham =
  [nop]

let () = Metapost.emit "automate_1" automate_1
let () = Metapost.emit "automate" automate
let () = Metapost.emit "loop_explain" loop_explain
let () = Metapost.emit "uml_client" uml_client
let () = Metapost.emit "uml" uml
let () = Metapost.emit "graph_sqrt" graph_sqrt
let () = Metapost.emit "architecture" architecture
let () = Metapost.emit "bresenham" bresenham
let () = Metapost.emit "tree" sharing
let () = Metapost.emit "arrow_metapost" arrow_metapost
let () = Metapost.emit "arrow_simple" arrow_simple
let () = Metapost.emit "arrow_loop_explain" arrow_loop_explain
let () = Metapost.emit "stages" stages
let () = Metapost.emit "simple" simple
let () = Metapost.emit "align" align
let () = Metapost.emit "persistance" persistance

