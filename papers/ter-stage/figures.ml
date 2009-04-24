
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

(* blocks mémoire *)     

let simple_block =
  let b = Box.hblock ~pos:`Bot [Box.tex "a"; Box.tex "b"; Box.tex "D"] in
  Box.draw b

let () = Metapost.emit "simple_block" simple_block


(*
Local Variables: 
compile-command: "make figures.mp"
End: 
*)
