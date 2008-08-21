open Path

let fig =
  let pen = Pen.circle ~tr:[Transform.scaled (Num.bp 2.)] () in
  let triangle = 
    path ~scale:Num.cm ~style:JLine ~cycle:JLine [(0.,0.);(1.,0.);(0.,1.)]
  in
    [ Command.draw ~pen triangle;
      Command.fill ~color:(Color.gray 0.8) triangle]
