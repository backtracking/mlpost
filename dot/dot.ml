open Xdot_type
open Mlpost

let parse_file f =
  let f  = open_in f in
  let f = Lexing.from_channel f in
  let d = Xdot_lexer.main f in
  d

module Pi = Picture

let ip (x,y) = (*Format.printf "%i,%i@." x y;*) Point.bpp (float_of_int x,float_of_int y)

let interp_node n = 
  let t = Pi.tex n.nname in
  let t = Pi.shift (ip n.npos) t in
  t

(* http://lists.cairographics.org/archives/cairo/2009-April/016916.html *)
open Num
open Command
module P = Point

(*
let bezier_of_bspline l =
  let spline = Array.of_list l in
  let q0 = P.scale (bp (1./.6.0)) (P.add (P.add spline.(0) (P.scale (bp 4.0) spline.(1))) spline.(2)) in
  let lastpt =  Array.length spline - 3 in
  let path = ref (MetaPath.start (MetaPath.knotp q0)) in
  for i = 0 to lastpt-1 do
    let p1 = spline.(i + 1) in
    let p2 = spline.(i + 2) in
    let p3 = spline.(i + 3) in

    let q1 = P.add (P.scale (bp (4.0/.6.0)) p1) (P.scale (bp (2.0/.6.0)) p2) in
    let q2 = P.add (P.scale (bp (2.0/.6.0)) p1) (P.scale (bp (4.0/.6.0)) p2) in
    let q3 =  P.scale (bp (1./.6.0)) (P.add (P.add p1 (P.scale (bp 4.0) p2)) p3) in
    path := MetaPath.concat ~style:(MetaPath.jControls q1 q2) (!path) (MetaPath.knotp q3)
  done;
  MetaPath.to_path !path
*)
  
let bezier_of_point_list = function
  | [] -> invalid_arg "Need at least one point"
  | a::l ->
      let rec aux acc = function
        | [] -> acc
        | [_]|[_;_] -> invalid_arg "not enough point (k*3 +1)"
        | a::b::c::l -> aux (MetaPath.concat ~style:(MetaPath.jControls a b) acc (MetaPath.knotp c)) l in
      MetaPath.to_path (aux (MetaPath.start (MetaPath.knotp a)) l)

let interp_spline l =  
  let l = List.map ip l in
  let p = bezier_of_point_list l in
  p

let interp_path = function
  | Plain -> assert false
  | Ellipse (p,w,h) -> assert false
  | Poly _ -> assert false
  | Bspline l -> interp_spline l

let interp_edge e = Path.draw (interp_path e.epath)

  
let dot_to_mlpost d = 
  let nodes = seq (List.map interp_node d.nodes) in
  let edges = seq (List.map interp_edge d.edges) in
  nodes++edges

let  xdot_to_mlpost f = 
  let d =  parse_file f in
  (*Format.printf "d = %a@." print_digraph d;*)
  dot_to_mlpost d

open Format

let print_nodes fmt l =
  List.iter (fun (n,w,h) -> fprintf fmt "%s [width=%f,height=%f];@." n (w/.72.) (h/.72.)) l

let print_edges fmt l =
  List.iter (fun (x,y) -> fprintf fmt "%s -> %s;@." x y) l

let print_dot fmt nodes edges = 
  fprintf fmt
    "@[<hov 1>digraph G {@[<hov 2>
node [label=\"\",shape=\"box\"];
edge [dir=none];
@[<hov 2>%a@]
@[<hov 2>%a@]
@]}@]" print_nodes nodes print_edges edges

let call_dot nodes edges =
  let ((pin,pout) as p) = Unix.open_process "tee example_in.log | dot -Txdot |tee example_out.log" in
  let pout2 = formatter_of_out_channel pout in
  print_dot pout2 nodes edges;
  pp_print_flush pout2 ();
  flush pout;
  close_out pout;
  let pin = Lexing.from_channel pin in
  let d = Xdot_lexer.main pin in
  match Unix.close_process p with
    | Unix.WEXITED 0 -> d
    | _ -> invalid_arg ("Dot doesn't like this graph") 

(** User interface *)

module type Box =
sig
  type abstract
  type concret
  val width : abstract -> Num.t
  val height : abstract -> Num.t
  val center : Point.t -> abstract -> concret
end

module Make (B : Box) =
struct
  type node = { id : int;
                fig : B.abstract}
      
  type edge = node * node

  let rec assoc_node n = function
    | [] -> raise Not_found
    | a::_ when a.id = n -> a.fig
    | _::l -> assoc_node n l

  let mknode = 
    let c = ref (-1) in
    fun x -> incr c;
    {id = !c; fig = x}

  let mkedge s e = (s,e)
    
  let mkedges l = l

  let node_name id = Format.sprintf "node%i" id
  let node_id s = Scanf.sscanf s "node%i" (fun id -> id)


  let place nodes edges =
    let cadd,compute = Concrete.compute_nums () in
    List.iter (fun n -> cadd (B.width n.fig);cadd (B.height n.fig)) nodes;
    compute ();
    let nodes2 = List.map 
      (fun n -> node_name n.id,
         Concrete.float_of_num (B.width n.fig),
         Concrete.float_of_num (B.height n.fig)) nodes in
    let edges = List.map (fun (n1,n2) -> (node_name n1.id,node_name n2.id)) edges in
    let d = call_dot nodes2 edges in
    (*printf "d.nodes : %i@.d.edges : %i" (List.length d.nodes) (List.length d.edges);*)
    let nodes = List.map (fun {nname = n; npos = p} ->
                            let fig = assoc_node (node_id n) nodes in
                            B.center (ip p) fig) d.nodes in
    let edges = List.map (fun {epath = p} -> interp_path p) d.edges in
    (nodes,edges)
end
      
  


