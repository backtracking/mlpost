(* mlpost -contrib dot dot.ml *)

open Mlpost
open Mlpost_dot

module Pi = Picture
open Command

(*parse <<togglescript>> *)

(*parse <<dot1 *)

module PiBox : Dot.Box 
  with type abstract = Pi.t
with type concrete = Pi.t = 
struct
  type abstract = Pi.t
  type concrete = Pi.t
  include Pi
end

module G = Dot.Make(PiBox)

let dot1 =
  let a = G.mknode (Pi.tex "Bonjour voici un A") in
  let b = G.mknode (Pi.tex "Bonjour voici un $\\frac{B}{B}$") in
  let c = G.mknode (Pi.tex "Bonjour voici un C") in
  let d = G.mknode (Pi.tex "Au revoir d") in
  let edges = G.mkedges [a,b;b,c;c,a;d,b;d,c;a,d] in
  let (nodes,edges) = G.place [a;b;c;d] edges in
  (seq nodes) ++ (seq (List.map Arrow.simple edges))

(*parse >> <<dot2 *)
module BBox : Dot.Box 
  with type abstract = Box.t
with type concrete = Box.t = 
struct
  type abstract = Box.t
  type concrete = Box.t
  include Box
end

module G2 = Dot.Make(BBox)

let dot2 =
  let tex s = Box.rect (Box.tex s) in
  let a = G2.mknode (tex "Bonjour voici un A") in
  let b = G2.mknode (tex "Bonjour voici un $\\frac{B}{B}$") in
  let c = G2.mknode (tex "Bonjour voici un C") in
  let d = G2.mknode (tex "Au revoir d") in
  let edges = G2.mkedges [a,b;b,c;c,a;d,b;d,c;a,d] in
  let (nodes,edges) = G2.place [a;b;c;d] edges in
  (seq (List.map Box.draw nodes)) ++ (seq (List.map Arrow.simple edges))

(*parse >> *)


let () = List.iter (fun (n,f) -> Metapost.emit n f)
  ["dot1",dot1;
   "dot2",dot2]
