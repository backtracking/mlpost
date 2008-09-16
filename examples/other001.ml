open Path

let fig = 
  let l = [0.,0.; 1., 0.; 0., 1.] in
    [ Command.draw (path ~style:jLine ~scale:Num.cm l)  ]
