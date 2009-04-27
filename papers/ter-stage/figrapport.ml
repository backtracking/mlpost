open Mlpost
open Num
open Color
open Box
open Path
open Tree
open Command

(* blocks mémoire *)     

let simple_block =
  let b = Box.hblock ~pos:`Bot [Box.tex "a"; Box.tex "A"; Box.tex "1"; Box.tex "$\\pi$"] in
  Box.draw b
    
(* traffic lights *)
let traffic =
  let two = Num.bp 2. in
  let b = 
    vbox ~fill:black ~padding:(Num.bp 3.) ~dx:two ~dy:two
      [ tex ~style:Circle ~fill:red "R";
	tex ~style:Circle ~fill:yellow "Y";
	tex ~style:Circle ~fill:green "G"; ]
  in
  Box.draw b
    
(* rubik's cube *)
let alpha = atan 1.
let beta = atan 1. /. 2. 
let mag = 10.
  
let proj x y z = 
  mag *. float (x - y) *. cos alpha, 
  mag *. (float (x + y) *. sin alpha *. sin beta +. float z *. cos beta)
    
let pen = Pen.scale (bp 2.5) Pen.default
  
let square color p i j =
  let pt i j = let x,y = p i j in Point.pt (bp x, bp y) in
  let points = [pt i j; pt (i+1) j; pt (i+1) (j+1); pt i (j+1)] in
  let path = pathp ~style:jLine ~cycle:jLine points in
  seq [fill ~color path; Command.draw ~pen path]
    
let right = square Color.orange (fun i j -> proj i 0 j)
let up = square Color.yellow (fun i j -> proj i j 3)
let left = square Color.green (fun i j -> proj 0 (3 - i) j)
  
let rubik = 
  seq [iter 0 2 (fun i -> iter 0 2 (right i));
       iter 0 2 (fun i -> iter 0 2 (up i));
       iter 0 2 (fun i -> iter 0 2 (left i));]
    
    
(* schema de l'interface *)
let interface = 
  let dx = bp 5. and dy = bp 5. in
  let tex' = tex ~style:RoundRect ~dx ~dy in
  let tex = tex' ~stroke:(Some Color.black) in
  let box name = box ~stroke:None ~dx:(mm 2.) ~name in
  let fml = Box.shift (Point.pt ((bp 0.),(bp 0.))) (box "fml" (tex "figure.ml")) in
  let fedit = Box.shift (Point.pt ((bp 100.),(bp 0.))) (box "fedit" (tex "figure.edit")) in
  let gmlpost = Box.shift (Point.pt ((bp 200.),(bp 0.))) (box "gmlpost" (tex "GMLPost")) in
  let glexer = Box.shift (Point.pt ((bp 100.),(bp (-75.)))) (box "glexer" (tex "glexer.mll")) in
  let mlpost = Box.shift (Point.pt ((bp 100.),(bp (75.)))) (box "mlpost" (tex "MLPost")) in
  let png = Box.shift (Point.pt ((bp 0.),(bp (75.)))) (box "png" (tex "figure.png")) in
  let a1 = Command.draw_arrow ~color:Color.green (pathp [(Box.north_west glexer);(Box.south_east fml)]) in
  let a2 = Command.draw_arrow ~color:Color.green (pathp [(Box.south_east fml);(Box.north_west glexer)]) in
  let a3 = Command.draw_arrow (pathp [(Box.north_east glexer);(Box.south_west gmlpost)]) in
  let a4 = Command.draw_arrow ~color:Color.green (pathp [(Box.north glexer);(Box.south fedit)]) in
  let a5 = Command.draw_arrow (pathp [(Box.west mlpost);(Box.east png)]) in
  let a6 = Command.draw_arrow (pathp [(Box.north_west gmlpost);(Box.south_east mlpost)]) in
  let a7 = Command.draw_arrow (pathp [(Box.south_west gmlpost);(Box.north_east glexer)]) in
  (Box.draw fml)++(Box.draw fedit)++(Box.draw gmlpost)++(Box.draw png)
  ++(Box.draw glexer)++(Box.draw mlpost)++a1++a2++a3++a4++a5++a6++a7
  

let hist1 =
  Hist.simple [3.;1.;6.]

let hist2 =
  Hist.compare
  [[1.;5.;6.;5.;3.];
   [1.;2.;3.;6.;-1.]]

let hist3 =
  Hist.stack ~fill:[lightred;lightblue;lightyellow;lightgreen]
    [[4.;5.;5.;]; [8.;3.;1.]; [2.;8.;1.;4.]]

let hist5 =
  Hist.stack
    ~perspective:true ~padding:(bp 25.)
    ~fill:[lightred;lightblue;lightyellow;lightgreen]
    ~histlabel:(`Center, Hist.Values)
    [[4.;5.;5.;]; [8.;3.;1.]; [2.;8.;1.;4.]]


let hist4 =
  let pics =
    List.map Picture.tex ["2000";"2001";"2002";"2003";"2004";"2005"]
  in
  Hist.simple ~histlabel:(`Top, Hist.User pics)
    [4.5;5.0;6.2;8.;7.2;6.1]

let radar1 =
  let pic =
    Radar.stack
      ~pen:(Pen.scale (bp 3.) Pen.circle)
      ~color:[blue;red;green]
      ~label:["weight";"acceleration";"speed";"maniability";"stickiness"]
      [[3.;4.;5.;6.;4.];
       [6.;5.;2.;1.;1.];
       [1.;7.;2.;4.;5.]]
  in
  Command.draw_pic pic
 

let radar2 =
    let pics =
    Radar.compare
    ~pen:(Pen.scale (bp 1.5) Pen.circle)
    ~color:[lightblue;lightred;lightgreen] ~fill:true
    [[3.;4.;5.;6.;4.];
      [6.;5.;2.;1.;1.];
      [1.;7.;2.;4.;5.]]
    in
    Box.draw (Box.vbox ~padding:(bp 10.) 
    (List.map (Box.pic ~stroke:None) pics))

let path1 = 
  let p = Path.smart_path 
    [Right;Up;Right]
    ~style:jLine
    (Point.pt (bp 0.,bp 0.)) 
    (Point.pt (bp 50.,bp 50.))
  in
  Command.draw p

let path2 = 
  let p = Path.smart_path 
    [Right;Down;Left;Down;Right;Down;Left;Down;Right]
    ~style:jLine
    (Point.pt (bp 0.,bp 100.)) 
    (Point.pt (bp 100.,bp 0.))
  in
  Command.draw_arrow p

let path3 = 
  let p = Path.smart_path 
    [Left;Down;Left;Down;Left] 
    (Point.pt (bp 0.,bp 0.)) 
    (Point.pt (bp (-50.),bp (-50.)))
  in
  Command.draw_arrow p


let path4 = 
  let p = Path.smart_path 
    [Down;Right;Upn (bp 10.);Right;Downn (bp 10.);Right;Upn (bp 10.);Right;Downn (bp 10.);Right;Upn (bp 10.);Right;Down]
    (Point.pt (bp 0.,bp 100.)) 
    (Point.pt (bp 100.,bp 0.))
  in
  Command.draw p

 
let tree1 =
  let node s = Tree.node ~edge_style:Curve (Box.tex s) in
  let leaf s = Tree.leaf (Box.tex s) in
  Tree.draw (node "1" [node "2" [leaf "4"; leaf "5"]; 
		       node "3" [leaf "6"; leaf "7"]])


let tree2 =
  let node s = Tree.node ~arrow_style:Undirected ~edge_style:HalfSquare (Box.tex s) in
  let leaf s = Tree.leaf (Box.tex s) in
  Tree.draw 
    (node "1" [node "2" [node "4" [leaf "8"]; leaf "5"]; 
	       node "3" [node "6" [leaf "9"; node "10" [leaf "12"; leaf "13"]]; 
			 node "7" [leaf "11"]]])


let leaf s = Tree.leaf (Box.set_stroke Color.black (Box.tex s))
let node s l = Tree.node  
  ~arrow_style:Undirected 
  ~edge_style:Straight 
  ~ls:(bp 30.)
  (Box.set_stroke Color.black (Box.tex s)) l
let subtree = node "subroot" [leaf "son1"; leaf "son2"; leaf "son3"]
let treebox = Box.rect (to_box subtree)
let maintree = node "root" [subtree; leaf "son"; Tree.node treebox [subtree]]
let tree3 = Tree.draw maintree 

let _ =
  List.iter (fun (name,fig) -> Metapost.emit name fig)
  [ "hist1", hist1;
    "hist2", hist2;
    "hist3", hist3;
    "hist4", hist4;
    "hist5", hist5;
    "radar1",radar1;
    "radar2",radar2;
    "simple_block",simple_block;
    "interface",interface;
    "path1",path1;
    "path2",path2;
    "path3",path3;
    "path4",path4;
    "tree1",tree1;
    "tree2",tree2;
    "tree3",tree3;
    "traffic",traffic;
    "rubik",rubik
  ]
