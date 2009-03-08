(**************************************************************************)
(*  cairo-ocaml -- Objective Caml bindings for Cairo                      *)
(*  Copyright © 2004-2005 Olivier Andrieu                                 *)
(*                                                                        *)
(*  This code is free software and is licensed under the terms of the     *)
(*  GNU Lesser General Public License version 2.1 (the "LGPL").           *)
(**************************************************************************)

type point = Cairo.point = 
    { x : float ; 
      y : float }

type one_spl = {
  pt : point array;
  mutable active : int;
}

type spl = {
    mutable pm           : GDraw.pixmap ;
    mutable spls      	 : one_spl list;
    mutable tolerance  	 : float ;
    mutable line_width 	 : float ;
            line_cap   	 : Cairo.line_cap ;
    mutable zoom       	 : float ;
    mutable xtrans     	 : float ;
    mutable ytrans     	 : float ;
    mutable click      	 : bool ;
    mutable drag_pt    	 : point ;
    mutable width        : int ;
    mutable height       : int ;
    mutable need_update  : bool ;
    mutable myfun_active : bool ;
            myfun        : point array list -> point list ;
  }


let ribbon = 
  [| 110., 20.  ; 310., 300. ;
     10. , 310. ; 210., 20.  |]

let nb_feature = 5
let select_feature = ref 0
let inter_depth = ref 11


let spline_copy arr =
  {pt = Array.map
    (fun (x, y) -> { x = x ; y = y })
    arr;
   active = 0}

let exec_on_spls f spls =
  f (List.map (fun {pt = pt} -> pt) spls) 

let new_pixmap width height =
  let drawable = GDraw.pixmap ~width ~height () in
  drawable#set_foreground `WHITE ;
  drawable#rectangle
    ~x:0 ~y:0 ~width ~height ~filled:true () ;
  drawable

let init_spl myfun = 
  let width = 400 in
  let height = 400 in
  {
   pm = new_pixmap width height ;
   spls = [spline_copy ribbon] ;
   tolerance = 0.1 ;
   line_width = 10. ; 
   line_cap = Cairo.LINE_CAP_ROUND ;
   zoom = 1. ;
   xtrans = 0. ;
   ytrans = 0. ;
   click = false ;
   drag_pt = { x = 0. ; y = 0. } ;
   width = width ;
   height = height ;
   need_update = true ;
   myfun_active = true;
   myfun = myfun;
 }



let draw_control_line cr a b w =
  Cairo.save cr ; begin
    Cairo.set_source_rgb cr 0. 0. 1. ;
    Cairo.set_line_width cr w ;
    Cairo.move_to cr a.x a.y ;
    Cairo.line_to cr b.x b.y ;
    Cairo.stroke cr end ;
  Cairo.restore cr

let two_pi = 8. *. atan 1.

let draw_spline cr spl one_spl =
  let drag_pt = { x = spl.drag_pt.x ; y = spl.drag_pt.y } in
  let drag_pt = Cairo.device_to_user cr drag_pt in
  Cairo.save cr ; begin
    Cairo.move_to cr  one_spl.pt.(0).x one_spl.pt.(0).y ;
    Cairo.curve_to cr 
      one_spl.pt.(1).x one_spl.pt.(1).y 
      one_spl.pt.(2).x one_spl.pt.(2).y 
      one_spl.pt.(3).x one_spl.pt.(3).y ;
    
    if spl.click && Cairo.in_stroke cr drag_pt
    then one_spl.active <- 0xf ;

    Cairo.stroke cr ;

    draw_control_line cr one_spl.pt.(0) one_spl.pt.(1) (2. /. spl.zoom) ;
    draw_control_line cr one_spl.pt.(3) one_spl.pt.(2) (2. /. spl.zoom) ;

    for i=0 to 3 do
      Cairo.save cr ; begin
	Cairo.set_source_rgba cr 1. 0. 0. 0.5 ;
	Cairo.new_path cr ;
	Cairo.arc cr 
	  one_spl.pt.(i).x one_spl.pt.(i).y
	  (spl.line_width /. 1.25)
	  0. two_pi ;
	if spl.click && Cairo.in_fill cr drag_pt
	then begin
	  one_spl.active <- 1 lsl i ;
	end ;
	Cairo.fill cr end ;
      Cairo.restore cr
    done end ;
  Cairo.restore cr
	  
let draw_point spl cr pt =
  Cairo.save cr ;
  Cairo.set_source_rgba cr 0. 1. 0. 0.5 ;
  Cairo.new_path cr ;
  Cairo.arc cr 
    pt.x pt.y
    (spl.line_width /. 1.25)
    0. two_pi ;
  Cairo.fill cr;
  Cairo.restore cr

let paint spl =
  let cr = Cairo_lablgtk.create spl.pm#pixmap in
  spl.pm#rectangle ~x:0 ~y:0 
    ~width:spl.width ~height:spl.height ~filled:true () ;
  Cairo.set_source_rgb cr 0. 0. 0. ;
  Cairo.set_line_width cr spl.line_width ;
  Cairo.set_line_cap cr spl.line_cap ;
  Cairo.translate cr spl.xtrans spl.ytrans ;
  Cairo.scale cr spl.zoom spl.zoom ;
  Cairo.set_tolerance cr spl.tolerance ;

  (try 
    List.iter (draw_spline cr spl) spl.spls ; 
    spl.need_update <- false
  with Cairo.Error _ ->
    prerr_endline "Cairo is unhappy");
  if spl.click then spl.click <- false;
  if spl.myfun_active then
    List.iter (draw_point spl cr) (exec_on_spls spl.myfun spl.spls)

let trans_horiz_cb dir spl =
  let delta = float spl.width /. 16. in
  begin match dir with
  | `LEFT  -> spl.xtrans <- spl.xtrans -. delta
  | `RIGHT -> spl.xtrans <- spl.xtrans +. delta
  end ;
  true

let trans_vert_cb dir spl =
  let delta = float spl.height /. 16. in
  begin match dir with
  | `UP   -> spl.ytrans <- spl.ytrans -. delta
  | `DOWN -> spl.ytrans <- spl.ytrans +. delta
  end ;
  true

let zoom_cb dir spl = 
  begin match dir with
  | `OUT -> spl.zoom <- spl.zoom /. 1.1 
  | `IN  -> spl.zoom <- spl.zoom *. 1.1 
  end ; 
  true

let smooth_cb dir spl =
  begin match dir with
  | `INC -> spl.tolerance <- spl.tolerance *. 10.
  | `DEC -> spl.tolerance <- spl.tolerance /. 10.
  end ; 
  true

let line_width_cb dir spl =
  begin match dir with
  | `W -> spl.line_width <- spl.line_width *. 2.
  | `N -> spl.line_width <- spl.line_width /. 2.
  end ; 
  true

let gest_spline action spl =
  begin match action with
    | `ADD -> spl.spls <- (spline_copy ribbon)::spl.spls
    | `REMOVE -> spl.spls <- (match spl.spls with [] -> [] | _::l -> l)
  end;
  true

let pt_f fmt p =
  Format.fprintf fmt "{@[ %.20g,@ %.20g @]}" p.x p.y

let print_spline =
  fun pt ->
    Format.printf "@[{ %a,@ %a,@ %a,@ %a }@]@." 
      pt_f pt.(0) pt_f pt.(1) pt_f pt.(2) pt_f pt.(3)

let print_one_spl {pt = pt} = print_spline pt

let print_spline_cb { spls = spls; myfun = myfun } =
  List.iter print_one_spl spls;
  List.iter (Format.printf "@[%a@]@." pt_f) (exec_on_spls myfun spls);
  Format.printf "depth : %i@." !inter_depth;
  false

module K = GdkKeysyms

let keybindings = [
  K._q,          ("Q",       (fun _ -> GMain.quit () ; false),
		  "Exit the program") ;
  K._Left,  	 ("Left",    trans_horiz_cb `LEFT,
		  "Translate left") ;
  K._Right, 	 ("Right",   trans_horiz_cb `RIGHT,
		  "Translate right" ) ;
  K._Up,         ("Up",      trans_vert_cb `UP,
		  "Translate up" ) ;
  K._Down,       ("Down",    trans_vert_cb `DOWN,
		  "Translate down") ;
  K._Return,     ("Return",  print_spline_cb,
		  "Print current spline coordinates on stdout") ;
  K._plus,       ("plus",    zoom_cb `IN,
		  "Zoom in") ;
  K._minus,      ("minus",   zoom_cb `OUT,
		  "Zoom out") ;
  K._greater,    ("greater", smooth_cb `DEC,
		  "Increase rendering accuracy, (tolerance /= 10)") ;
  K._less,       ("less",    smooth_cb `INC,
		  "Decrease rendering accuracy, (tolerance *= 10)") ;
  K._w,          ("W",       line_width_cb `W,
		  "Widen line width") ;
  K._n,          ("N",       line_width_cb `N,
		  "Narrow line width") ;
  K._a,          ("A",       gest_spline `ADD,
		  "Add a spline") ;
  K._r,          ("R",       gest_spline `REMOVE,
		  "Remove a spline") ;
  K._f,          ("F",  (fun spl -> spl.myfun_active<-not spl.myfun_active;true),
		  "Switch the fun fun") ;
  K._c,          ("C",  (fun _ -> select_feature := (!select_feature + 1) mod nb_feature;true),
		  "Change the green points which appear") ;
  K._d,          ("d",  (fun _ -> inter_depth := max 0 (!inter_depth-1);true),
		  "Change the depth of the intersection") ;
  K._D,          ("D",  (fun _ -> incr(inter_depth);true),
		  "Change the depth of the intersection") ;
]

let refresh da spl =
  spl.need_update <- true ;
  GtkBase.Widget.queue_draw da#as_widget

let grow_pixmap spl =
  spl.pm <- new_pixmap spl.width spl.height ;
  spl.need_update <- true 
  (* no need to queue a redraw here, an expose 
     event should follow the configure, right ? *)

let config_cb spl ev =
  let w = GdkEvent.Configure.width ev in
  let h = GdkEvent.Configure.height ev in
  let has_grown = w > spl.width || h > spl.height in
  spl.width <- w ;
  spl.height <- h ;
  if has_grown
  then grow_pixmap spl ;
  true

let expose da spl x y width height =
  let gwin = da#misc#window in
  let d = new GDraw.drawable gwin in
  d#put_pixmap ~x ~y ~xsrc:x ~ysrc:y ~width ~height spl.pm#pixmap

let expose_cb da spl ev =
  let area = GdkEvent.Expose.area ev in
  let module GR = Gdk.Rectangle in
  if spl.need_update then paint spl ;
  expose da spl (GR.x area) (GR.y area) (GR.width area) (GR.height area) ;
  true



let key_press_cb da spl ev =
  try 
    let (_, cb, _) =
      List.assoc (GdkEvent.Key.keyval ev) keybindings in
    let need_refresh = cb spl in
    if need_refresh then refresh da spl ;
    true
  with Not_found -> false

let button_ev da spl ev =
  match GdkEvent.get_type ev with
  | `BUTTON_PRESS ->
      spl.click <- true ;
      spl.drag_pt <- { x = GdkEvent.Button.x ev ; y = GdkEvent.Button.y ev } ;
      true
  | `BUTTON_RELEASE -> 
      spl.click  <- false ;
      List.iter (fun one_spl -> one_spl.active <- 0)  spl.spls;
      true
  | _ -> false

let motion_notify_cb da spl ev =
  let x = GdkEvent.Motion.x ev in
  let y = GdkEvent.Motion.y ev in
  List.iter
  (fun one_spl ->
  for i=0 to 3 do
    if (1 lsl i) land one_spl.active != 0
    then begin
      let x = one_spl.pt.(i).x +. (x -. spl.drag_pt.x) /. spl.zoom in
      let y = one_spl.pt.(i).y +. (y -. spl.drag_pt.y) /. spl.zoom in
      one_spl.pt.(i) <- { x = x ; y = y }
    end
  done ;) spl.spls;
  spl.drag_pt <- { x = x ; y = y } ;
  refresh da spl ;
  true


let init spl packing =
  let da = GMisc.drawing_area ~width:spl.width ~height:spl.height ~packing () in
  da#misc#set_can_focus true ;
  da#event#add [ `KEY_PRESS ;
		 `BUTTON_MOTION ;
		 `BUTTON_PRESS ; `BUTTON_RELEASE ] ;
  ignore (da#event#connect#expose         (expose_cb da spl)) ;
  ignore (da#event#connect#configure      (config_cb spl));
  ignore (da#event#connect#button_press   (button_ev da spl)) ;
  ignore (da#event#connect#button_release (button_ev da spl)) ;
  ignore (da#event#connect#motion_notify  (motion_notify_cb da spl)) ; 
  ignore (da#event#connect#key_press      (key_press_cb da spl))

let show_help kb =
  Format.printf "@[<v>" ;
  List.iter (fun (_, (key, _, descr)) ->
    Format.printf "%10s: %s@ " key descr)
    kb ;
  Format.printf "@."


let f4 f a b c d = f (f a b) (f c d)

let cubic a b c d t =
  d*.(t**3.)+.3.*.c*.(t**2.)*.(1.-.t)+.3.*.b*.(t**1.)*.(1.-.t)**2.+.a*.(1.-.t)**3.

let cubic_point a t =
  {x=cubic a.(0).x a.(1).x a.(2).x a.(3).x t;
   y=cubic a.(0).y a.(1).y a.(2).y a.(3).y t;}

let extremum a b c d =
  let sol delta = (delta -. (2.*.b) +. a +. c)/.(a -. d +. (3.*.(c -. b))) in
  let delta = ((b*.b) -. (c*.(b +. a -. c)) +. (d*.(a -. b))) in
  match compare delta 0. with
    | x when x<0 -> []
    | 0 -> [sol 0.]
    | _ -> [sol (delta**0.5);sol (-.(delta**0.5))]

let remarquable a b c d = (0.::(1.::(List.filter (fun x -> x>=0. && x<=1.) (extremum a b c d))))

(** simple intersection *)
let give_bound a =
  let x_max = f4 max a.(0).x a.(1).x a.(2).x a.(3).x in
  let y_max = f4 max a.(0).y a.(1).y a.(2).y a.(3).y in
  let x_min = f4 min a.(0).x a.(1).x a.(2).x a.(3).x in
  let y_min = f4 min a.(0).y a.(1).y a.(2).y a.(3).y in
  (x_min,y_min,x_max,y_max)

let give_bound_precise a =
  let x_remarq = List.map (cubic a.(0).x a.(1).x a.(2).x a.(3).x) (remarquable a.(0).x a.(1).x a.(2).x a.(3).x) in
  let y_remarq = List.map (cubic a.(0).y a.(1).y a.(2).y a.(3).y) (remarquable a.(0).y a.(1).y a.(2).y a.(3).y) in
  let x_max = List.fold_left max neg_infinity x_remarq in
  let y_max = List.fold_left max neg_infinity y_remarq in
  let x_min = List.fold_left min infinity x_remarq in
  let y_min = List.fold_left min infinity y_remarq in
  (x_min,y_min,x_max,y_max)

let test_in amin amax bmin bmax =
  (amin <= bmax && bmin <= amax)

let is_intersect a b =
  let (ax_min,ay_min,ax_max,ay_max) = give_bound a in
  let (bx_min,by_min,bx_max,by_max) = give_bound b in
  test_in ax_min ax_max bx_min bx_max &&
  test_in ay_min ay_max by_min by_max

let is_intersect_precise a b =
  let (ax_min,ay_min,ax_max,ay_max) = give_bound_precise a in
  let (bx_min,by_min,bx_max,by_max) = give_bound_precise b in
  test_in ax_min ax_max bx_min bx_max &&
  test_in ay_min ay_max by_min by_max

let add a b = {x = a.x+.b.x; y = a.y+.b.y}
let minus a b = {x = a.x-.b.x; y = a.y-.b.y}
let half a = {x = a.x/.2.; y = a.y/.2.}
let double a = {x = a.x*.2.; y = a.y*.2.}
let add_half a b = half (add a b)

let bisect a =
  let add_half b i = b.(i) <- half (add b.(i) b.(i-1)) in
  let add_halfr b i = b.(i) <- half (add b.(i) b.(i+1)) in
  let b = Array.copy a in
  add_half b 3;(*D\leftarrow (C+D)/2*)
  add_half b 2;add_half b 3;(*C\leftarrow (B+C)/2, D\leftarrow (C+D)/2*)
  add_half b 1;add_half b 2;add_half b 3;
 (*B\leftarrow (A+B)/2, C\leftarrow (B+C)/2, D\leftarrow(C+D)/2*)
  let c = Array.copy a in
  add_halfr c 0;
  add_halfr c 1;add_halfr c 0;
  add_halfr c 2;add_halfr c 1;add_halfr c 0;
  (b,c)


let give_intersect a b =
  let rec aux a b l t1 t2 dt= function
      | 0 -> if is_intersect a b then (t1+.(dt/.2.),t2+.(dt/.2.))::l else l
      | n -> let n = n-1 in
	let dt = dt/.2. in
(*	print_spline a;
	print_spline b;*)
    if is_intersect a b then
      let (af,al) = bisect a in
      let (bf,bl) = bisect b in
      let l = aux af bf l t1 t2 dt n in
      let l = aux af bl l t1 (t2+.dt) dt n in
      let l = aux al bf l (t1+.dt) t2 dt n in
      let l = aux al bl l (t1+.dt) (t2+.dt) dt n in
      l
    else l in
  let rem_noise delta (which:float*float -> float) noisy =
    let sort = List.fast_sort (fun x y -> compare (which x) (which y)) noisy in
    let rec group (s1,s2) a n = function
      | [] -> [s1/.n,s2/.n]
      | ((e1,e2) as e)::l when (which e)-.a<=delta -> group (s1+.e1,s2+.e2) (which e) (n+.1.) l
      | e::l -> (s1/.n,s2/.n)::(group e (which e) 1. l) in
    match sort with
      |[] -> []
      |a::l -> group a (which a) 1. l in
  let l = aux a b [] 0. 0. 1. !inter_depth in
  let delta = 0.5**(float_of_int (!inter_depth -1)) in
  rem_noise delta snd (rem_noise delta fst l)

let rec keep f a = function
    | [] -> a
    | b::l -> if f a b then keep f a l else keep f b l

(** Knuth solution (with some simplifications) *)
type red_bez = { u1 : point;
		 u2 : point;
		 u3 : point;
		 umax : point;
		 umin : point;
		 ut : float}
let min_max v1 v2 v3 =
  let aux u1 u2 u3 =
    if u1<0. then
      if u3>=0. then
	(if u2 < 0. then u1+.u2 else u1),max 0. (u1+.u2+.u3)
      else
	(min u1 (u1+.u2+.u3),max 0. u1+.u2)
    else
      if u3<=0. then
	(min 0. (u1+.u2+.u3),if u2>0. then u1+.u2 else u1)
      else
	(min 0. (u1+.u2),max u1 (u1+.u2+.u3)) in
  let (xmin,xmax) = aux v1.x v2.x v3.x in
  let (ymin,ymax) = aux v1.y v2.y v3.y in
  ({x=xmin;y=ymin},{x=xmax;y=ymax})


let leq {x=xa;y=ya} {x=xb;y=yb} = xa <= xb && ya <= yb

let leqleq a b c = leq a b && leq b c

let bisect2 w dt = (* 559 *)
  let w1l = w.u1 in
  let w3r = w.u3 in
  let w2l = add_half w1l w.u2 in
  let w2r = add_half w3r w.u2 in
  let w3l = add_half w2l w2r in
  let w1r = w3l in
  let (wminl,wmaxl) = min_max w1l w2l w3l in
  let (wminr,wmaxr) = min_max w1r w2r w3r in
  ({u1 = w1l;u2 = w2l; u3 = w3l; umax = wmaxl;umin=wminl; ut = w.ut},
   {u1 = w1r;u2 = w2r; u3 = w3r; umax = wmaxr;umin=wminr; ut = w.ut +. dt})

let cubic_intersection p pp =(*550*)
  let rec aux w z d l n dt =(*556*)
    if not (leqleq (minus z.umin w.umax) d (minus z.umax w.umin))
    then l
    else if n=0 then
      (w.ut,z.ut)::l
      else
      (* 559 *)
      let n = n-1 in
      let dt = dt/.2. in
      let (wl,wr) = bisect2 w dt in
      let (zl,zr) = bisect2 z dt in
      let dll = double d in
      let l = aux wl zl dll l n dt in
      let drl = add (add (add dll wl.u1) wl.u2) wl.u3 in
      let l = aux wr zl drl l n dt in
      let drr = minus (minus (minus dll zl.u1) zl.u2) zl.u3 in
      let l = aux wr zr drr l n dt in
      let dlr = minus (minus (minus d zl.u1) zl.u2) zl.u3 in
      let l = aux wl zr dlr l n dt in
      l
  in
  (*558*)
  let w1 = minus p.(1) p.(0) in
  let w2 = minus p.(2) p.(1) in
  let w3 = minus p.(3) p.(2) in
  let (wmin,wmax) = min_max w1 w2 w3 in
  let w = {u1=w1;u2=w2;u3=w3;umin=wmin;umax=wmax;ut=0.} in
  let z1 = minus p.(1) p.(0) in
  let z2 = minus p.(2) p.(1) in
  let z3 = minus p.(3) p.(2) in
  let (zmin,zmax) = min_max z1 z2 z3 in
  let z = {u1=z1;u2=z2;u3=z3;umin=zmin;umax=zmax;ut=0.} in
  let d = minus p.(0) pp.(0) in
  aux w z d [] (!inter_depth) 1.

(** *)

(* Prend une liste de splines en argument et renvoit une liste de points à afficher *)
(*[start;start_control;end_control;end]*)
let myfun (spls:point array list) : point list =
  let map f = List.map (function [|a;b;c;d|] -> f a b c d | _ -> assert false) in
  match !select_feature with
    | 0 ->  map (fun pstart _ _ pend -> 
                   {x = (pstart.x+.pend.x)/.2.; y = (pstart.y+.pend.y)/.2.}) spls
    | 1|2 -> List.concat (map (fun a b c d -> 
                               let f az bz cz dz = List.map (cubic_point [|a;b;c;d|]) (remarquable az bz cz dz) in 
                let xextr = (f a.x b.x c.x d.x) in
                let xmax = keep (fun a b -> a.x > b.x) {x=neg_infinity;y=0.} xextr in
                let xmin = keep (fun a b -> a.x < b.x) {x=infinity;y=0.} xextr in
                let yextr = (f a.y b.y c.y d.y) in
                let ymax = keep (fun a b -> a.y > b.y) {x=0.;y=neg_infinity} yextr in
                let ymin =keep (fun a b -> a.y < b.y) {x=0.;y=infinity} yextr in
                  if !select_feature=2 then 
                    xextr@yextr else [xmax;xmin;ymax;ymin]) spls)
    | 3 -> (match spls with
	| [a;b] -> List.map (fun (tp,_) -> cubic_point a tp) (give_intersect a b)
	| _ -> [])
    | 4 -> (match spls with
	      | [a;b] -> List.map (fun (tp,_) -> cubic_point a tp) (cubic_intersection a b)
	      | _ -> [])
    | _ -> assert false



let main = 
  let w = GWindow.window 
      ~title:"Cairo spline demo" 
      ~allow_grow:true
      ~allow_shrink:true
      () in
  ignore (w#connect#destroy GMain.quit) ;
  init (init_spl myfun) w#add ;
  show_help keybindings ;
  w#show () ;
  GMain.main ()
