open Mlpost
open Num
open Box

let fig =
  let b = 
    hbox ~padding:(bp 20.) ~pos:`Bot
      [rect (tex "A");
       vbox [rect (tex "B"); tex "C"]]
  in
  draw (vbox [b;b;b])

let () = Metapost.emit "fig" fig

let triple x = [x;x;x]
let nine x = triple (triple x)
let u = bp 10.

let sudoku =
  let e = rect (empty ~width:u ~height:u ()) in
  let b = tabularl ~hpadding:zero ~vpadding:zero (nine e) in
  let pen = Pen.scale (pt 1.5) Pen.circle in
  let b = rect ~pen ~dx:zero ~dy:zero b in
  draw (tabularl (nine b))

let () = Metapost.emit "sudoku" sudoku

