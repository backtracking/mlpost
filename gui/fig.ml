open Mlpost
open Num
open Box
open Tree
open Command

module Edit = struct
  open Glexer

  let file = "fig.edit"
  let elements = ref []
  let table = Hashtbl.create 17

  let () = 
    if Sys.file_exists file then begin
      elements := Glexer.read_file file;
      let add = function
	| Num (s,_) | Point (s,_,_) as e -> Hashtbl.add table s e
      in
      List.iter add !elements
    end

  let mk_num = function
    | Pt -> Num.pt
    | Cm -> Num.cm
    | Mm -> Num.mm
    | Bp -> Num.bp
    | Inch -> Num.inch

  let num s v dimension =
    try match Hashtbl.find table s with
      | Num (_, (f, dim)) -> mk_num dim f
      | Point _ -> invalid_arg ("already a point of name " ^ s)
    with Not_found ->
      let e = Num (s, (v, dimension)) in
      elements := e :: !elements;
      Hashtbl.add table s e;
      mk_num dimension v

  let point s v1 dim1 v2 dim2 =
    try match Hashtbl.find table s with
      | Num _ -> invalid_arg ("already a num of name " ^ s)
      | Point (_,(n1,d1),(n2,d2)) -> Point.pt (mk_num d1 n1,mk_num d2 n2)
    with Not_found ->
      let e = Point (s, (v1,dim1), (v2,dim2)) in
      elements := e :: !elements;
      Hashtbl.add table s e;
      Point.pt(mk_num dim1 v1,mk_num dim2 v2)
	
  let () =
    at_exit (fun () -> Glexer.write_file file !elements)

end

let ls = Edit.num "ls" 5. Glexer.Cm
let cs = Edit.num "cs" 5. Glexer.Cm
let p1 = Edit.point "p1" 50. Glexer.Bp 50. Glexer.Bp
let p2 = Edit.point "p2" 100. Glexer.Bp 100. Glexer.Bp
let chemin = Path.pathp [p1;p2]

let node = node ~ls ~cs

let t =
  node (tex "A")
    [node (tex "B") [];
     node (tex "C") []]

let () = Metapost.emit "tree1" 
  (Box.draw (shift p1 (Tree.to_box t)) ++(Box.draw (Box.empty ~stroke:(Some Color.red) ~width:(bp 50.) ~height:(bp 50.) ())) ++ (draw chemin))


