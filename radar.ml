open Command
open Path
open Num
open Color
open Types
open Infix


(* Module implÅÈmentant une file avec deux listes *)
module Q = Misc.Q


let scale_radius r l=
  List.map (fun (x,y) -> (multf x r,multf y r)) l
let scale_radius_2 r l = 
  List.map (scale_radius r) l

(* Calcule la liste des max des iÅËme ÅÈlÅÈments de chaque liste *)
let maxlist =function
  |ml::ll->
     List.fold_left (List.map2 max) ml ll
  |[]-> failwith "Empty list"

type direction = Horizontal | Vertical | Other

(* Calcule les 2 points Å‡ distance d de la droite de coefficient directeur a2_equ perpendiculaire Å‡ l'axe de tc *)
let rec make_paths a2_equ tc d sens acc radius = match tc with
  |(absc,ordo)::res -> 
     let (absc,ordo) = (multf absc radius,multf ordo radius) in
       begin
	 match sens with
	   | Vertical -> (* Le long de l'axe des ordonnÅÈes *)
	      let absc2 = (+/) absc d in
	      let absc3 = (-/) absc d in
		make_paths a2_equ res d sens ([(absc2,ordo);(absc3,ordo)]::acc) radius
	   | Horizontal -> (* Le long de l'axe des abscisses *)
	      let ordo2 = (+/) ordo d in
	      let ordo3 = (-/) ordo d in
		make_paths a2_equ res d sens ([(absc,ordo2);(absc,ordo3)]::acc) radius
	   | Other -> 
	      let b2_equ = (-/) ordo (( *./) a2_equ absc) in
	      let co = 1. /. (sqrt (1.+.a2_equ*.a2_equ)) in
	      let angle = if a2_equ>0. then 360 - (int_of_float ((acos co)*.180./.pi)) else int_of_float (acos(co)*.180./.pi) in
	      let absc2 = (+/) absc (( *./) (cos ((float angle)*.2.*.pi/.360.)) d) in
	      let ordo2 = (+/) (( *./) a2_equ absc2) b2_equ in
	      let angle2 = (angle+180) mod 360 in
	      let absc3 = (+/) absc (( *./) (cos ((float angle2)*.2.*.pi/.360.)) d) in
	      let ordo3 = (+/) (( *./) a2_equ absc3) b2_equ in
		make_paths a2_equ res d sens ([(absc2,ordo2);(absc3,ordo3)]::acc) radius
       end
  |[]-> acc

(* Dessine les ticks le long de l'axe passÅÈ en paramÅËtre *)
let draw_ticks ticks coords m d radius=
  let (x,y) = List.hd (List.rev coords) in
  let rec ticks_coords acc ticks i x y m =
    if i<=m then ticks_coords ((x*.i/.m,y*.i/.m)::acc) ticks (i+.ticks) x y m
    else acc
  in
  let tc = ticks_coords [] ticks ticks x y m in
  let x = if (abs_float x < 10e-4) then 0. else x in
  let y = if (abs_float y < 10e-4) then 0. else y in
  let a2_equ,sens = 
    if x=0. then 0.,Vertical else if y=0. then 0.,Horizontal else ((-.x)/.y),Other
  in 
  let p = make_paths a2_equ tc d sens [] radius in 
    iterl (fun x -> draw (pathn x)) p

(* *)
let draw_label pt lab radius = 
  let (x,y) = List.hd (List.rev pt) in
  let angl = (acos (x /. (sqrt (x*.x +. y*.y))))*.180./.pi in
  let angle = if y<0. then 360.-.angl else angl in
  let placement = 
    if ((angle>315. && angle<360.) || (angle>=0. && angle<=45.)) then `Right
    else if (angle>45. && angle<=135.) then `Top
    else if (angle>135. && angle<=225.) then `Left
    else `Bot
  in
    Command.label ~pos:placement (Picture.tex lab) (Point.pt (multf x radius, multf y radius))


(* Dessine le radar vide *)
let rec draw_skeleton acc ?label ticks lmax skltn d radius= 
  let label = match label with
    |None -> []
    |Some i -> i
  in
  match skltn,lmax,label with
    |x::res,m::lm,lab::labl -> let x2= scale_radius radius x in
	draw_skeleton ((draw (pathn x2))
		       ++(draw_ticks ticks x m d radius)
		       ++(draw_label x lab radius)++acc) ~label:labl ticks lm res d radius
    |x::res,m::lm,[] -> let x2= scale_radius radius x in
	draw_skeleton ((draw (pathn x2))
		       ++(draw_ticks ticks x m d radius)++acc) ~label:[] ticks lm res d radius
    |[],[],[] -> acc
    |_,_,_-> failwith "Different list sizes"


(* Fabrique une liste contenant les coordonnÅÈes des axes du radar *)
let empty_radar_coords nbr = 
  let delta = 360. /. (float nbr) in 
  let rec empty_radar acc nb diff angle = 
    if nb>0 then 
      empty_radar ([(0.,0.);(cos (angle*.2.*.pi/.360.), sin (angle*.2.*.pi/.360.) )]::acc) 
	(nb-1) diff (angle+.diff)
    else List.rev acc 
  in
    empty_radar [] nbr delta 0.


(* Fabrique la liste des coordonnÅÈes correspondant Å‡ chaque valeur *)
let list_coord lmax l skeleton =
  let rec fct lmax l skeleton acc =
    match lmax,l,skeleton with
      |x::res,y::res2,z::res3 -> 
	 let (z1,z2) = List.hd (List.rev z) in
	 let x_coord = z1*.y/.x in
	 let y_coord = z2*.y/.x in
	   fct res res2 res3 ((x_coord,y_coord)::acc)
      |[],[],[] -> List.rev acc
      |_,_,_ -> failwith "Different list sizes"
  in
    fct lmax l skeleton []

(* Fabrique un radar associÅÈ au squelette de radar passÅÈ en paramÅËtre *)
let radar color lmax l skeleton track fill stl radius= 
  let coords = scale_radius radius (list_coord lmax l skeleton) in
  let rec dots acc f c = match c with
    |x::res -> 
       let col = if f then Color.black else color in
       let cmd = draw ~pen:(Pen.scale (bp 5.) Pen.circle) ~color:col (pathn [x]) in
	 dots (cmd++acc) f res
    |[]->acc
  in 
  let dots_cmd = dots nop fill coords in
  let clr = if fill then Color.black else color in
  let path_cmd = draw (pathn ~style:jLine ~cycle:jLine coords) ~pen:track ~color:clr ~dashed:stl in
  let path_filled = if fill then (Command.fill ~color:color (pathn ~style:jLine ~cycle:jLine coords)) else nop 
  in
    path_filled++path_cmd++dots_cmd



let init ?radius ?scale ?style ?pen l=
 let radius = match radius with
    |None -> bp 100.
    |Some i -> i
  in
  let ticks_size = divf (multf 3. radius) 100. in
  let lesmax = match scale with
    |None -> maxlist l
    |Some l -> l 
  in
  let skeleton =
    match l with
      |x::_ -> empty_radar_coords (List.length x)
      |[] -> failwith "No data"
  in
  let style = 
    match style with
      |Some i -> i
      |None -> [(Dash.pattern [Dash.on (bp 1.);Dash.off (bp 0.)])]
  in
  let pen = 
    match pen with
      |Some i -> i
      |None -> (Pen.scale (bp 0.5) Pen.circle)
  in
    radius,pen,style,ticks_size,lesmax,skeleton
  

  
(* Fabrique des radars empilÅÈs *)
let stack ?radius ?(color=[black]) ?pen ?style ?(ticks=1.) ?label ?scale l =
  
  let radius,pen,style,ticks_size,lesmax,skeleton = init ?radius ?scale ?style ?pen l in
    
  let rec radar_list col stl maxi li skltn acc = match li,col,stl with
    |x::res,cq,sq ->
       let c,cres = Q.pop cq in
       let s,sres = Q.pop sq in
	 radar_list (Q.push c cres) (Q.push s sres) maxi res skltn 
	   ((radar c maxi x skltn pen false s radius)++acc)
    |[],cq,sq-> acc
  in Picture.make ((draw_skeleton nop ?label ticks lesmax skeleton ticks_size radius)
		   ++(radar_list (Q.of_list color) (Q.of_list style) lesmax l skeleton nop))


(* Fabrique des radars comparatifs, renvoie la liste de Pictures reprÅÈsentant chaque radar *)
let compare ?radius ?(color=[black]) ?(fill=false) ?pen ?style ?(ticks=1.) ?label ?scale l =
  
  let radius,pen,style,ticks_size,lesmax,skeleton = init ?radius ?scale ?style ?pen l in

  let rec build_pictures skltn col stl maxi li tcks acc = match li,col,stl with
    |x::res,cq,sq -> 
       let c,cres = Q.pop cq in
       let s,sres = Q.pop sq in
       let r = radar c maxi x skltn pen fill s radius in
       let sk = draw_skeleton nop ?label tcks maxi skltn ticks_size radius in
       let pic = Picture.make (r++sk) in
	 build_pictures skltn (Q.push c cres) (Q.push s sres) maxi res tcks (pic::acc)
    |[],cq,sq-> List.rev acc
  in 
    build_pictures skeleton (Q.of_list color) (Q.of_list style) lesmax l ticks [] 
  



