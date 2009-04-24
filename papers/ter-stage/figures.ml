
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

(* schema de l'interface *)
let interface = 
  let dx = bp 5. and dy = bp 5. in
  let tex' = tex ~style:RoundRect ~dx ~dy in
  let tex = tex' ~stroke:(Some Color.black) in
  let box name = box ~stroke:None ~dx:(mm 2.) ~name in
  let fml = box "fml" (tex "figure.ml") in
  let fedit = box "fedit" (tex "figure.edit") in
  let png = box "png" (tex "figure.png") in
  nop

let () = Metapost.emit "simple_block" simple_block
let () = Metapost.emit "interface" interface

(*
Local Variables: 
compile-command: "make figures.mp"
End: 
*)
