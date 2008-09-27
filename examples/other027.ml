open Command

let a = Point.pt (Num.bp 0., Num.bp 0.)
let pen = Pen.circle ~tr:[Transform.scaled (Num.bp 4.)] () 
let fig = [
  draw (Path.pathp [a]) ~pen;
  label ~pos:`Top (Picture.tex "Au dessus") a;
  label ~pos:`Bot (Picture.tex "En dessous") a;
  label ~pos:`Right (Picture.tex "\\`A droite") a;
  label ~pos:`Left (Picture.tex "\\`A gauche") a;
]
