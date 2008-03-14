
open Types

let rec path = function
  | PASub (f1, f2, p) ->
      let p,code = path p in
      let n = Name.path () in
      PASub (f1, f2, PAName n), CSeq [code; CDeclPath (n, p)]
  | p -> 
      p, CSeq []

let rec command = function
  | CDraw (p, color, pen, dash) ->
      let p,code = path p in
      CSeq [code; CDraw (p, color, pen, dash)]
  | c -> 
      c
