open Mlpost
open Num
open Color
open Box
open Path
open Tree
open Command



(***************** Simple Block ******************)


let simple_block =
  let b = Box.hblock ~pos:`Bot [Box.tex "a"; Box.tex "A"; Box.tex "1"; Box.tex "$\\pi$"] in
  Box.draw b
    
    
(***************** Traffic Lights ******************)    
    

let traffic =
  let two = Num.bp 2. in
  let b = 
    vbox ~fill:black ~padding:(Num.bp 3.) ~dx:two ~dy:two
      [ tex ~style:Circle ~fill:red "R";
	tex ~style:Circle ~fill:yellow "Y";
	tex ~style:Circle ~fill:green "G"; ]
  in
  Box.draw b
    
    
(***************** Rubik's Cube ******************)
    
    
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
    
    
(***************** Histograms ******************)
    
    
let hist1 =
  Hist.simple
    ~width:(bp 100.)
    ~height:(bp 200.)
    [3.;1.;6.]
    
let hist2 =
  Hist.compare
    ~width:(bp 100.)
    ~height:(bp 200.)
    [[1.;5.;6.;5.;3.];
     [1.;2.;3.;6.;-1.]]
    
let hist3 =
  let vlabel _ _ = None in
    Hist.stack 
      ~vlabel
      ~fill:[lightred;lightblue;lightyellow;lightgreen]
      [[4.;5.;5.;]; [8.;3.;1.]; [2.;8.;1.;4.];[1.5;3.5];[2.;2.;7.;1.]]
    
let hist5 =
  let vlabel _ _ = None in
  let rot s = Picture.rotate 25. (Picture.tex ("\\large{"^s^"}")) in
  Hist.stack
    ~vlabel
    ~width:(bp 100.)
    ~height:(bp 200.)
    ~perspective:true ~padding:(bp 15.)
    ~fill:[lightred;lightblue;lightyellow;lightgreen]
    ~histlabel:(`Center, Hist.Values)
    ~hlabel:[rot "first";rot "second";rot "third" ]
    [[4.;5.;5.;]; [8.;3.;1.]; [2.;8.;1.;4.]]
    

let hist4 =
  let pics =
    List.map Picture.tex ["2000";"2001";"2002";"2003";"2004";"2005"]
  in
  Hist.simple 
    ~width:(bp 150.)
    ~height:(bp 270.)
    ~histlabel:(`Top, Hist.User pics)
    ~hcaption:(Picture.tex "Year")
    [4.5;5.0;6.2;8.;7.2;6.1]


(***************** Radars ******************)


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


(***************** Paths ******************)


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
    [Right;Down;Left;Down(* ;Right;Down;Left;Down;Right *)]
    ~style:jLine
    (Point.pt (bp 0.,bp 0.)) 
    (Point.pt (bp 0.,bp (-30.)))
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

 
(***************** Trees ******************)


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


(***************** GMlpost ******************)


let init = 
  let dx = bp 5. and dy = bp 5. in
  let tex' = tex ~style:RoundRect ~dx ~dy  in
  let tex = tex' ~stroke:(Some Color.black) in
  let box name = box ~stroke:None ~dx:(mm 2.) ~name in
  let fml = box "fml" (tex ".ml") in
  let gmlpost = box "gmlpost" (tex "GMLPost") in
  let mlpost = box "mlpost" (tex "MLPost") in 
  let glexer = box "glexer" (tex "Glexer") in
  let edit = box "edit" (tex "Edit") in
  let png = box "png"(tex ".png") in
  let fedit = box "fedit" (tex ".edit")
  in
  fml,gmlpost,mlpost,glexer,edit,png,fedit

let interface = 
  let fml,gmlpost,mlpost,glexer,edit,png,fedit = init in
  let boxarray = Array.make_matrix 4 3 (Box.empty ()) in
  boxarray.(0).(0) <-  fml ;
  boxarray.(1).(0) <-  gmlpost ;
  boxarray.(1).(1) <-  mlpost ;
  boxarray.(1).(2) <-  edit ;
  boxarray.(3).(2) <-  glexer ;
  boxarray.(2).(1) <-  png ;
  boxarray.(3).(1) <-  fedit ;
  tabular ~hpadding:(bp 20.) ~vpadding:(bp 20.)  boxarray 

let interface1 = 
  let mbox = interface in
  let a1 = Command.draw_arrow ~color:Color.blue 
    (pathp [(Box.south (Box.get "fml" mbox));(Box.north (Box.get "gmlpost" mbox))]) in
  let a2 = Command.draw_arrow ~color:Color.blue 
    (pathp [(Box.east (Box.get "gmlpost" mbox));(Box.west (Box.get "mlpost" mbox))]) in
  let a3 = Command.draw_arrow ~color:Color.blue 
    (pathp [(Box.south (Box.get "mlpost" mbox));(Box.north (Box.get "png" mbox))]) in
  let a4 = Command.draw_arrow ~color:Color.blue 
    (pathp [(Box.east (Box.get "mlpost" mbox));(Box.west (Box.get "edit" mbox))]) in
  let a5 = Command.draw_arrow ~color:Color.blue 
    (pathp [(Box.south (Box.get "edit" mbox));(Box.north (Box.get "glexer" mbox))]) in
  let a6 = Command.draw_arrow ~color:Color.blue 
    (pathp [(Box.west (Box.get "glexer" mbox));(Box.east (Box.get "fedit" mbox))]) in
  let a7 = Command.draw_arrow ~color:Color.blue 
    (smart_path ~style:jLine [Left;Up] (Box.west (Box.get "fedit" mbox)) (Box.south (Box.get "gmlpost" mbox))) in
  let a8 = Command.draw_arrow ~color:Color.blue 
    (smart_path ~style:jLine [Left;Up] (Box.west (Box.get "png" mbox)) (Box.south (Box.get "gmlpost" mbox))) in
  (Box.draw mbox)++a1++a2++a3++a4++a5++a6++a7++a8

let interface2 = 
  let mbox = interface in
  let a1 = Command.draw_arrow ~color:Color.red 
    (smart_path ~style:jLine [Down;Right] (Box.south (Box.get "gmlpost" mbox)) (Box.west (Box.get "fedit" mbox))) in
  let a2 = Command.draw_arrow ~color:Color.red 
    (pathp [(Box.south (Box.get "mlpost" mbox));(Box.north (Box.get "png" mbox))]) in
  let a3 = Command.draw_arrow ~color:Color.red 
    (smart_path ~style:jLine [Left;Up;Left] (Box.west (Box.get "png" mbox)) (Box.east (Box.get "gmlpost" mbox))) in
  let a4 = Command.draw_arrow ~color:Color.red 
    (smart_path ~style:jLine [Rightn (bp 20.);Up;Leftn (bp 5.)] (Box.east (Box.get "fedit" mbox)) (Box.east (Box.get "mlpost" mbox))) in
  (Box.draw mbox)++a1++a2++a3++a4


(***************** Legend ******************)
let legend1 = 
  let l = Legend.legend 
    [(Color.lightgreen,"2009");(Color.lightyellow,"2010");(Color.lightred,"2011")]
  in Command.draw_pic l


(***************** ******************)


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
    "interface1",interface1;
    "interface2",interface2;
    "path1",path1;
    "path2",path2;
    "path3",path3;
    "path4",path4;
    "tree1",tree1;
    "tree2",tree2;
    "tree3",tree3;
    "traffic",traffic;
    "rubik",rubik;
    "legend1",legend1
  ]
