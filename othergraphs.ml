open Path
open Point
open Helpers
open Mlpost
module C = Convenience
module P = Pen
module T = Transform

let a = 0., 0.
let b = 1., 0.
let c = 0., 1.
let l = [a ; b ; c]
let d1 = 1, [C.draw ~style:JLine ~scale:C.CM l]
let d2 = 2, [C.draw ~style:JLine ~scale:C.CM ~cycle:JLine 
                l]

let d4 = 4, 
         let pen = P.transform [T.scaled (Num.bp 4.)] Pen.circle in
           [C.draw ~pen:pen ~scale:C.CM [a] ]

let d5 = 5,
         let pen = P.transform [T.scaled (Num.bp 4.)] Pen.circle in
           [C.draw ~style:JLine ~scale:C.CM ~cycle:JLine l] @
           (List.map  (fun point -> C.draw ~pen:pen ~scale:C.CM [point]) l)

let d12 = 12,
          let pen = P.transform [T.scaled (Num.bp 2.)] Pen.circle in
          let cl = 
            List.map (fun f -> Color.make (Color.RGB (f,f,f) ) ) 
              [0.8;0.6;0.4] 
          in
            List.map2
              (fun (a,b) col ->
                 C.draw ~style:JLine ~scale:C.CM ~pen:pen ~color:col [a;b])
              [a,b;b,c;c,a] cl


let figs = 
  [ d1; d2; d4; d5; d12]

let mpostfile = "test/othergraphs.mp"
let texfile = "test/othergraphs.tex"

let _ =
    Generate.generate_mp mpostfile figs;
    Generate.generate_tex texfile "othergraphs/mpost" "othergraphs" figs


