
open Helpers

type node = Box.t

type node_style = Circle | Rect

let fortybp x = Num.bp (40. *. x)

let node ?(style=Circle) ?(scale=fortybp) x y s = 
  let p = Point.p (scale x, scale y) in
  let pic = Picture.tex s in
  match style with Circle -> Box.circle p pic | Rect -> Box.rect p pic

type dir = Up | Down | Left | Right | Angle of float

type arrow = { 
  src : node; 
  dst : node; 
  lab : string; 
  pos : Command.position option;
  outd : dir option;
  ind : dir option;
}

type t = {
  nodes : node list;
  mutable arrows: arrow list;
}

let create l = 
  { nodes = l; arrows = [] }

let arrow d ?(lab="") ?pos ?outd ?ind n1 n2 =
  d.arrows <- 
    { src = n1; dst = n2; lab = lab; pos = pos; outd = outd; ind = ind } 
    :: d.arrows

let outdir = function
  | Up -> Path.Vec Point.up
  | Down -> Path.Vec Point.down
  | Left -> Path.Vec Point.left
  | Right -> Path.Vec Point.right
  | Angle f -> Path.Vec (Point.dir f)

let indir = function
  | Up -> Path.Vec Point.down
  | Down -> Path.Vec Point.up
  | Left -> Path.Vec Point.right
  | Right -> Path.Vec Point.left
  | Angle f -> Path.Vec (Point.dir f)

let outdir = function None -> None | Some x -> Some (outdir x)
let indir = function None -> None | Some x -> Some (indir x)

let draw_arrow ?stroke ?pen a =
  if a.lab = "" then 
    box_arrow 
      ?color:stroke ?pen ?outd:(outdir a.outd) ?ind:(indir a.ind) a.src a.dst
  else
    box_label_arrow 
      ?color:stroke ?pen ?outd:(outdir a.outd) ?ind:(indir a.ind) 
      ?pos:a.pos (Picture.tex a.lab) a.src a.dst

let draw ?fill ?stroke ?pen d =
  List.map (Command.draw_box ?fill) d.nodes @
  List.map (draw_arrow ?stroke ?pen) d.arrows

