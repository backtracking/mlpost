open Mlpost
open Diag

let fig =
  let a = node 0. 0. "A" in
  let b = node 1. 0. "B" in
  let diag = create [a;b] in
  arrow diag a b;
  [draw diag]
