open Command
open Diag

let fig =
  let pen = Pen.circle () in
  let empty_node x y = node x y "\\phantom{M}" in
  let a = empty_node 0. 4. in
  let b = empty_node 0. 3. in
  let inv = node 0. 2. "inv" in
  let c = empty_node 0. 1. in
  let d = empty_node 0. 0. in
  let do_ = node (-2.) 2. "do" in
  let diag = create [a;b;c;d;inv;do_] in
  let arrow = arrow diag in
  arrow a b ~lab:"$i\\leftarrow0$" ~pos:`Right;
  arrow b inv ~lab:"$m\\leftarrow t[i]$" ~pos:`Right;
  arrow c d ~lab:"$i\\ge n$" ~pos:`Right;
  arrow c do_ ~outd:Left ~ind:Down ~lab:"$i<n$" ~pos:`Lowleft;
  arrow inv c ~lab:"$i\\leftarrow i+1$" ~pos:`Right;
  arrow do_ inv ~lab:"$m\\ge t[i]$" ~pos:`Top;
  arrow do_ b ~outd:Up ~ind:Left ~lab:"$m<t[i]$" ~pos:`Upleft;
  [draw ~fill:Color.yellow ~stroke:Color.blue ~pen diag]
