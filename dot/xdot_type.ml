type point = int * int

type path = 
  | Ellipse of point * int * int
  | Bspline of point list
  | Poly of point list
  | Plain

type node = 
    { nname : string;
      npos : point;
      nshape : path}

type edge =
    { estart : string;
      eend : string;
      epath : path}

type digraph = {bounding_box : point * point;
                nodes : node list;
                edges : edge list}

let digraph = {bounding_box = ((0,0),(0,0));
                nodes = [];
                edges = []}

open Format

let print_point fmt (x,y) =
  fprintf fmt "%i,%i" x y

let print_path fmt = function
  | Ellipse (p,w,h) -> fprintf fmt "E %a %i %i" print_point p w h
  | Bspline pl -> fprintf fmt "Bspline ";
      List.iter (fprintf fmt "%a;" print_point) pl
  | Poly pl ->fprintf fmt "Poly ";
      List.iter (fprintf fmt "%a;" print_point) pl
  | Plain -> fprintf fmt "Plain"

let print_node fmt n = 
  fprintf fmt "%s [pos=%a;shape=%a];@." n.nname print_point n.npos print_path n.nshape

let print_edge fmt n = 
  fprintf fmt "%s -> %s [%a];@." n.estart n.eend print_path n.epath


let print_digraph fmt d =
  List.iter (print_node fmt) d.nodes;
  List.iter (print_edge fmt) d.edges;
