open Mlpost
open Box
open Tree
let sprintf = Format.sprintf

let stern_brocot h =
  let frac (a,b) = tex (sprintf "$\\frac{%d}{%d}$" a b) in
  let rec make ((a,b) as lo) ((c,d) as hi) h =
    let r = a+c, b+d in
    if h = 1 then
      leaf (frac r)
    else
      node ~arrow_style:Undirected (frac r) [make lo r (h-1); make r hi (h-1)]
  in
  make (0,1) (1,0) h

let tree8 = draw (stern_brocot 5)

let texint n = tex (sprintf "$F_{%d}$" n)
let rec fib = function
  | 0 | 1 as n -> leaf (texint n)
  | n -> node ~arrow_style:Undirected (texint n) [fib (n-1); fib (n-2)]

let fibtree = draw (fib 6)

type t = | Node of int * t list

let rec bin = function
  | 0 -> Node (0, [])
  | n -> 
      let (Node (_,l) as t) = bin (n-1) in
      Node (n, t :: l)

let rec trans (Node (n,l)) = 
  node ~arrow_style:Undirected (tex (sprintf "${2^{%d}}$" n)) (List.map trans l)

let tree10 = draw (trans (bin 4))

open Num
open Color

let hist =
  Hist.stack ~fill:[lightred;lightblue;lightyellow;lightgreen]
    [[4.;5.;5.;]; [8.;3.;1.]; [2.;8.;1.;4.]]

open Helpers
open Command
open Box
let hbox = hbox ~padding:(bp 30.)
let vbox = vbox ~padding:(bp 30.)

let diag =
  let rt s = rect (tex s) in
  let a = rt "A" and b = rt "B" and c = rt "C" in
  let ab = round_rect ~dx:(bp 10.) ~dy:(bp 10.) (hbox [a;b]) in
  let v = vbox [ab;c] in
  let arrow x y = box_arrow ~sep:(bp 5.) (sub x v) (sub y v) in
  draw v ++ arrow a b ++ arrow ab c 

let _ = 
  List.iter (fun (name,fig) -> Metapost.emit name fig)
  [ 
    "fibtree", fibtree;
    "hist", hist;
    "diag", diag;
(*     "tree10", tree10; *)
  ]
