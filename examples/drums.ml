open Mlpost
open Point
open Command
open Path

let scale f = 2.0 *. Num.mm f

let drawover p = seq [fill ~color:Color.white p; draw p]
let filldraw p c = seq [fill ~color:c p; draw p]
let polyg fl = path ~scale ~style:JLine ~cycle:JLine fl

let mydraw b c p = if b then filldraw p c else drawover p

let copper = Color.rgb (213./.256.) (186./.256.) (6./.256.)
let steel = Color.gray 0.85

(** The (closed) hihat ! true : color, false : b&w *)
let hihat b =
  let d = mydraw b in
  let cymb = transform [Transform.xscaled (scale 7.);
			Transform.yscaled (scale 1.2)] fullcircle in
  let head =
    polyg [(-0.5,0.); (-0.3,0.); (-0.15,1.65); (0.15,1.65);
	   (0.3,0.); (0.5,0.); (0.33,-1.2); (-0.33,-1.2)] in
  let stem =
    polyg [(0.2,-1.2); (0.2,-9.); (-0.2,-9.); (-0.2,-1.2)] in
  let stemjoint =
    polyg [(0.5,-4.8); (0.25,-6.); (-0.25,-6.); (-0.5,-4.8)] in
  let foot b l alpha c =
    let hb = b /. 2. and ls = l*.(sin alpha) and lc = -.l*.(cos alpha) in
    let f = polyg [(hb,0.); (hb-.ls,lc); (-.hb-.ls,lc); (-.hb,0.)] in
      transform [Transform.shifted (p ~scale c)] f
  in
    seq [d steel stem; d steel stemjoint;
	 d steel (foot 0.5 3.5 0. (0.,-9.0));
	 d steel (foot 0.5 7.5 (Misc.deg2rad 35.) (-0.3,-9.0));
	 d steel (foot 0.5 7.5 (Misc.deg2rad (-35.)) (0.3,-9.0));
	 d steel (foot 0.3 2.1 (Misc.deg2rad 110.) (0.0,-12.));
	 d steel (foot 0.3 2. (Misc.deg2rad 250.) (0.0,-12.));
	 d steel (foot 0.3 5. 0. (0.,-9.0));
	 d steel head; d copper cymb] 

(** The snare drum ! true : color, false : b&w *)
let snare b =
  let d = mydraw b in
  let drumout = 
    polyg [(-3.5,1.66);(3.5,1.66);(3.5,-1.66);(-3.5,-1.66)] in
  let drumin =
    polyg [(-3.5,1.26);(3.5,1.26);(3.5,-1.26);(-3.5,-1.26)] in
    seq [d steel drumout; d Color.red drumin]
    
let snarepic b pos =
  let pic = Picture.make [snare b] in
  let tpic = Picture.transform [Transform.shifted (p ~scale pos)] pic in
    draw_pic tpic

let fig = 
  [hihat true; snarepic true (-5.3,-3.8)]
