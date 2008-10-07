
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
  let sk = Plot.mk_skeleton 4 2 u u in
  let label = Picture.tex "$\\sqrt x$", `Top, 3 in
  let graph = Plot.draw_func ~label (fun x -> sqrt (float x)) sk in
    [graph; Plot.draw_simple_axes "$x$" "$y$" sk]

let architecture =
  let mk_box fill name m = 
    Box.tex ~style:RoundRect ~dx:(bp 5.) ~dy:(bp 5.) ~name ~fill m in
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
  let compile = mk_unbox "compile" "Compile" in
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
  let box_ = mk_box fill "box" "Box" in
  let shapes = mk_box fill "shapes" "Shapes" in
  let arrows = mk_box fill "arrow" "Arrow" in
  let advanced = Box.hbox ~pos:`Bot ~padding:(mm 2.) [box_; shapes; arrows] in
    (* extensions *)
  let fill = Color.color "blanched almond" in 
  let tree = mk_box fill "tree" "Tree" in
  let diag = mk_box fill "diag" "Diag" in
  let plot = mk_box fill "plot" "Plot" in
  let extensions = Box.hbox ~pos:`Bot ~padding:(mm 2.) [tree; diag; plot] in
    (* wrapping *)
  let pyramid = 
    let pen = Pen.scale (Num.bp 1.0) (Pen.square ()) in
    Box.vbox ~padding:(mm 2.) ~pen
      ~dx:(bp 5.) ~dy:(bp 5.) ~style:RoundRect ~stroke:(Some Color.black)
      [extensions; advanced; basictypes; compile_ext; metapost_ext] in
  let mlpost = mk_unbox "mlpost" "Mlpost" in
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
(* Les lignes qui suivent ne sont pas à montrer dans l'article *)
module Arrow = ExtArrow
let loop box =
  let c = Box.ctr box in
  let a = Point.shift c (pt (cm 0., cm (-0.8))) in
  let p = Path.pathk [
    knotp ~r: (vec (dir 225.)) c;
    knotp a;
    knotp ~l: (vec (dir 135.)) c;
  ] in
  let bp = Box.bpath box in
  cut_after bp (cut_before bp p)

(* A partir d'ici on a le code qu'on peut montrer dans l'article *)
open Num
let state = Box.tex ~style:Circle 
let final = Box.box ~style:Circle ~dx:zero ~dy:zero 
let transition b tex pos ?outd ?ind x y = 
  let x = Box.get x b and y = Box.get y b in
  let outd = match outd with None -> None | Some a -> Some (vec (dir a)) in
  let ind = match ind with None -> None | Some a -> Some (vec (dir a)) in
  Arrow.draw ~tex ~pos (cpath ?outd ?ind x y) 
let loop b tex pos x = (* TODO : utiliser pos *)
  let x = Box.get x b in
  Arrow.draw ~tex ~pos (loop x)
let initial b pos x =
  let x = Box.get x b in
  let w = Box.west x in (* TODO : utiliser pos *)
  Arrow.draw (Path.pathp [ Point.shift w (Point.pt (cm (-0.3), zero)); w ])

let automate =
  let states = Box.vbox ~padding:(cm 0.8) [
    Box.hbox ~padding:(cm 1.4) [ 
      state ~name:"alpha" "$\\alpha$"; state ~name:"beta" "$\\beta$" ];
    final (state ~name:"gamma" "$\\gamma$"); ]
  in
  [Box.draw states;
   transition states "a" `Lowleft "alpha" "gamma";
   transition states "b" `Lowright "gamma" "beta";
   transition states "c" `Top ~outd:25. ~ind:335. "alpha" "beta";
   transition states "d" `Bot ~outd:205. ~ind:155. "beta" "alpha";
   loop states "e" `Bot "gamma";
   initial states `Left "alpha"]

(* Johannes *)
open Box
let uml = 
  let classblock name attr_list method_list = 
    let tex = Box.tex ~stroke:None in
    let vbox = Box.vbox ~pos:`Left in
      Box.vblock ~pos:`Left ~name
      [ tex ("{\\bf " ^ name ^ "}");
        vbox (List.map tex attr_list); vbox (List.map tex method_list)
      ]
  in
  let a = classblock "BankAccount" 
            [ "balance : Dollars = $0$"] 
            ["deposit (amount : Dollars)"; "withdraw (amount : Dollars)" ] 
  in
  let b = classblock "Client" ["name : String"; "address : String" ] [] in
  let diag = Box.vbox ~padding:(Num.bp 50.) [a;b] in
  [ Box.draw diag; 
    box_label_arrow ~pos:`Left (Picture.tex "owns") 
      (get "Client" diag) (get "BankAccount" diag) ]

let hierarchy =
  let two = Num.bp 2. in
  let five = Num.bp 5. in
  let tex = tex ~dx:two ~dy:two in
  let tex' = tex ~stroke:None in
  let vbox = vbox ~padding:(Num.bp 3.) ~stroke:(Some Color.black) 
                  ~style:RoundRect ~dy:five ~dx:five
  in
  let b = 
    vbox [ tex' "set of all languages";
      vbox [ tex' "recursively enumerable languages";
        vbox [ tex' "decidable languages";
          vbox [ tex' "context sensitive";
            vbox [tex' "context free"; tex ~style:RoundRect "regular" ]
    ] ] ] ]
  in
  [ draw b]


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



(* JC *)
let bresenham =
  [nop]

let () = Metapost.emit "automate" automate
let () = Metapost.emit "uml" uml
let () = Metapost.emit "hierarchy" hierarchy
let () = Metapost.emit "graph_sqrt" graph_sqrt
let () = Metapost.emit "architecture" architecture
let () = Metapost.emit "bresenham" bresenham
let () = Metapost.emit "tree" sharing
