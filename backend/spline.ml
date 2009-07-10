(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Johannes Kanig, Stephane Lescuyer                       *)
(*  Jean-Christophe Filliatre, Romain Bardou and Francois Bobot           *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

let _ = GMain.Main.init ()

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

let point_dist_min = {x = 100.;y=100.}

let nb_feature = 4
let select_feature = ref 0

let inter_depth = Spline_lib.inter_depth
let debug = Spline_lib.debug

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
	  
let draw_point spl cr col pt  =
  Cairo.save cr ;
  (match col with
    |`Green -> Cairo.set_source_rgba cr 0. 1. 0. 0.5 ;
    |`Yellow -> Cairo.set_source_rgba cr 0. 1. 1. 0.5);
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
    List.iter (draw_point spl cr `Green) (exec_on_spls spl.myfun spl.spls);
  draw_point spl cr `Yellow point_dist_min

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
  Format.printf "depth : %i@." !Spline_lib.inter_depth;
  false

module K = GdkKeysyms

let keybindings = [
  K._q,          ("q",       (fun _ -> GMain.quit () ; false),
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
  K._w,          ("w",       line_width_cb `W,
		  "Widen line width") ;
  K._n,          ("n",       line_width_cb `N,
		  "Narrow line width") ;
  K._a,          ("a",       gest_spline `ADD,
		  "Add a spline") ;
  K._r,          ("r",       gest_spline `REMOVE,
		  "Remove a spline") ;
  K._f,          ("f",  (fun spl -> spl.myfun_active<-not spl.myfun_active;true),
		  "Switch the fun fun") ;
  K._c,          ("c",  (fun _ -> select_feature := (!select_feature + 1) mod nb_feature;true),
		  "Change the green points which appear") ;
  K._d,          ("d",  (fun _ -> inter_depth := max 0 (!inter_depth-1);true),
		  "Change the depth for the intersection") ;
  K._D,          ("D",  (fun _ -> incr(inter_depth);true),
		  "Change the depth for the intersection") ;
  K._e,          ("e",  (fun _ -> debug:= not !debug;!debug),
		  "Print debug information") ;
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

let rec one_to_one f a = function
  | [] -> a
  | e::l -> one_to_one f (List.fold_left (f e) a l) l

let rec keep f a = function
    | [] -> a
    | b::l -> if f a b then keep f a l else keep f b l

(* Prend une liste de splines en argument et renvoit une liste de points à afficher *)
(*[start;start_control;end_control;end]*)
let myfun (spls:point array list) : point list =
  let map f = List.map (function 
                          | [|a;b;c;d|] -> 
                              let s = Spline_lib.create a b c d in
                              f s 
                          | _ -> assert false) in
  match !select_feature with
    | 0 -> one_to_one (fun a lpt b -> (List.map (fun (tp,_) -> Spline_lib.abscissa_to_point a tp) 
                                         (Spline_lib.intersection a b))@lpt) [] (map (fun x -> x) spls)
    | 1 -> map (fun s -> Spline_lib.abscissa_to_point s (Spline_lib.dist_min_point s point_dist_min)) spls
    | 2 -> one_to_one (fun a lpt b -> ((fun (tp,_) -> Spline_lib.abscissa_to_point a tp) 
                                         (Spline_lib.dist_min_path a b))::lpt) [] (map (fun x -> x) spls)
    | 3 -> one_to_one (fun a lpt b -> try
                         ((fun (tp,_) -> Spline_lib.abscissa_to_point a tp) 
                             (Spline_lib.one_intersection a b))::lpt
                       with Not_found -> lpt) [] (map (fun x -> x) spls)
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
