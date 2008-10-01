
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
  let fill = Color.color "salmon" in 
  let mk_box name m = 
    Box.tex ~style:RoundRect ~dx:(bp 5.) ~dy:(bp 5.) ~name ~fill m in
  let mk_unbox name m = 
    Box.tex ~style:RoundRect ~stroke:None ~dx:(bp 5.) ~dy:(bp 5.) ~name m in
  let num = mk_box "num" "Num" in
  let point = mk_box "point" "Point" in
  let path = mk_box "num" "Path" in
  let dots = mk_unbox "dots" "$\\ldots$" in
  let cmd = mk_box "cmd" "Command" in
  let basictypes = Box.hbox ~padding:(mm 2.) [num; point; path; dots; cmd] in
  let compile = mk_unbox "compile" "Compile" in
  let compile_ext = 
    let dx = (Box.width basictypes -/ Box.width compile) /./ 2. in
    Box.hbox ~style:RoundRect ~dx ~fill ~stroke:(Some Color.black) [compile]
  in
    [Box.draw (Box.vbox ~padding:(mm 2.) [basictypes; compile_ext])]

(* Romain *)
let automate =
  [nop]

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
  let tex' s = tex ~stroke:None in
  let vbox = vbox ~padding:(Num.bp 3.) ~stroke:(Some black) 
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

(* JC *)
let bresenham =
  [nop]

let () = Metapost.emit "automate" automate
let () = Metapost.emit "uml" uml
let () = Metapost.emit "hierarchy" hierarchy
let () = Metapost.emit "graph_sqrt" graph_sqrt
let () = Metapost.emit "architecture" architecture
let () = Metapost.emit "bresenham" bresenham




