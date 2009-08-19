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

let _ = 
  List.iter (fun (name,fig) -> Metapost.emit name fig)
  [ 
    "fibtree", fibtree;
(*     "tree10", tree10; *)
  ]
