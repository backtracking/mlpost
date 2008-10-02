(* Extended arrows. *)

let normalize p =
  Point.scale (Num.divn (Num.bp 1.) (Point.length p)) p

let neg = Point.scale (Num.bp (-1.))

(* Atoms *)

type line = {
  dashed: Types.dash option;
  color: Types.color option;
  pen: Types.pen option;
  from_point: float;
  to_point: float;
  dist: Num.t;
}

type body = line list

let body_empty = []

let add_line ?dashed ?color ?pen ?(from_point = 0.) ?(to_point = 1.)
    ?(dist = Num.bp 0.) body =
  {
    dashed = dashed;
    color = color;
    pen = pen;
    from_point = from_point;
    to_point = to_point;
    dist = dist;
  } :: body

type head = Point.t -> Point.t -> Command.t * Path.t

let head_classic ?color ?pen ?dashed ?(angle = 60.) ?(size = Num.bp 4.) p dir =
  let dir = Point.scale size dir in
  let dir_a = neg (Point.rotate (angle /. 2.) dir) in
  let dir_b = neg (Point.rotate (-. angle /. 2.) dir) in
  let a = Point.add p dir_a in
  let b = Point.add p dir_b in
  let path = Path.pathp ~style:Path.jLine [a; p; b] in
  Command.draw ?color ?pen ?dashed path, path

type belt = {
  clip: bool;
  rev: bool;
  point: float;
  head: head;
}

type kind = body * belt list

let kind_empty body = body, []

let add_belt ?(clip = false) ?(rev = false) ?(point = 0.5)
    ?(head = fun x -> head_classic x) (body, belts) =
  body, {
    clip = clip;
    rev = rev;
    point = point;
    head = head;
  } :: belts

let add_head ?head kind = add_belt ~clip: true ~point: 1. ?head kind

let add_foot ?head kind = add_belt ~clip: true ~rev: true ~point: 0. ?head kind

let body_simple = add_line body_empty

let simple = add_head (kind_empty body_simple)

(* Compute the path of a line along an arrow path.
   Return the line (unchanged) and the computed path. *)
let make_arrow_line path line =
  (* TODO: use line.dist, line.from_point and line.to_point *)
  line, path

(* Compute the command and the clipping path of a belt along an arrow path.
   Return the belt (unchanged), the command and the clipping path. *)
let make_arrow_belt path belt =
  (* TODO: divide the point by the length of the path, but we need another
     function Path.point which uses a Num.t instead of a float *)
  let p = Path.point belt.point path in
  let d = normalize (Path.direction belt.point path) in
  let d = if belt.rev then neg d else d in
  let command, clipping_path = belt.head p d in
  belt, command, clipping_path

(* Clip a line with a belt clipping path if needed. *)
let clip_line_with_belt (line, line_path) (belt, _, clipping_path) =
  (* TODO *)
  line, line_path

(* Compute the command to draw a line. *)
let draw_line (line, line_path) =
  Command.draw ?color: line.color ?pen: line.pen ?dashed: line.dashed line_path

let draw ?(kind = simple) ?tex ?pos path =
  let lines, belts = kind in
  let lines = List.map (make_arrow_line path) lines in
  let belts = List.map (make_arrow_belt path) belts in
  let lines =
    List.map (fun line -> List.fold_left clip_line_with_belt line belts) lines in
  let lines = List.map draw_line lines in
  let belts = List.map (fun (_, x, _) -> x) belts in
  let labels = match tex with
    | None -> []
    | Some tex -> [Command.label ?pos (Picture.tex tex) (Path.point 0.5 path)]
  in
  Command.seq (lines @ belts @ labels)

(* Non-Default Instances *)

let body_double =
  add_line ~dist: (Num.bp (-1.5)) (add_line ~dist: (Num.bp 1.5) body_empty)

let simple = add_head (kind_empty body_simple)

let double = add_head (kind_empty body_double)

let draw2 ?kind ?tex ?pos ?outd ?ind a b =
  let r, l = outd, ind in
  draw ?kind ?tex ?pos (Path.pathk [Path.knotp ?r a; Path.knotp ?l b])
