open Path

let simple ?style ?outd ?ind a b =
  let r,l = outd, ind in
   pathk ?style [knotp ?r a; knotp ?l b]
