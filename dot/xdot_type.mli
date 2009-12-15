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

val digraph : digraph


val print_point : Format.formatter -> point -> unit
val print_digraph : Format.formatter -> digraph -> unit
