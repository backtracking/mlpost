open GMain
open GdkKeysyms
open Glexer
open Format


let () = ignore (GtkMain.Main.init ())

let usage () =
  eprintf "usage: gmlpost file.ml fig-name@.";
  exit 1

let ml_file, fig_name = 
  if Array.length Sys.argv <> 3 then usage ();
  let f = Sys.argv.(1) in
  if not (Sys.file_exists f && Filename.check_suffix f ".ml") then usage ();
  f, Sys.argv.(2)

(* run the mlpost file and create PNG image *)

let sys_command cmd =
  let c = Sys.command cmd in
  if c <> 0 then begin
    eprintf "command '%s' failed with exit code %d@." cmd c;
    exit 1
  end

(* size parameters *)
let canvas_width = 500.
let canvas_height = 500.

let xmin = ref (-1.)
let xmax = ref (1.)
let ymin = ref (-1.)
let ymax = ref (1.)
let dx = ref 2.
let dy = ref 2.
let pic_w = ref 1.
let pic_h = ref 1.

let set_bbox xmi ymi xma yma =
  xmin := xmi;
  xmax := xma;
  ymin := ymi;
  ymax := yma;
  dx := !xmax -. !xmin;
  dy := !ymax -. !ymin

let update_bbox () =
  let file = fig_name ^ ".1" in
  let c = open_in file in
  try 
    while true do
      let s = input_line c in
      try
	Scanf.sscanf s "%%%%HiResBoundingBox: %f %f %f %f" (fun a b c d -> set_bbox a b c d; raise Exit)
      with Scanf.Scan_failure _ ->
	()
    done 
  with 
    | End_of_file ->
	eprintf "warning: could not find the bounding box in %s @." file;
	close_in c
    | Exit -> 
	close_in c

let make_png () =
  sys_command ("mlpost -ps -ccopt glexer.cmo " ^ ml_file);
  sys_command "latex tree1 > /dev/null";
  sys_command "dvips -E tree1.dvi > /dev/null";
  sys_command "convert tree1.ps tree1.png > /dev/null";
  update_bbox ()

let () = make_png ()

let png_file = fig_name ^ ".png"

let edit_file =
  Filename.chop_suffix ml_file ".ml" ^ ".edit"

let elements = Glexer.read_file edit_file

let pointstable = Hashtbl.create (List.length elements)

let rec init_table = function
  |[],_,_|_,[],_|_,_,[]|Point _::_ ,_::[],_-> ()
  | Num _::r,_::sps,ps -> 
     init_table (r,sps,ps)
  | Point (s,_,_)::r,sp1::sp2::sps,p::ps ->       
      Hashtbl.add pointstable s ((sp1,sp2),p) ;
      init_table (r,sps,ps)

let spins = ref []
let points = ref []
let z = ref 1.


let belong x y = (x>0. && x<canvas_width && y>0. && y<canvas_height)


  


(*------------------------------------------------------------------------*)

let rec go_string fmt = function
  |[],_|_,[]|Point _::_,_::[] -> ()
  | Num (s,(_,d))::r,sp::sps ->
      let n = sp#value in
	fprintf fmt "num \"%s\" %f %s \n" s n (string_of_dim d);
	go_string fmt (r,sps)
  | Point (s,(_,d1),(_,d2))::r,sp1::sp2::sps ->
      let n1 = sp1#value in
      let n2 = sp2#value in
	fprintf fmt "point \"%s\" %f %s , %f %s \n" s 
	  n1 (string_of_dim d1) 
	  n2 (string_of_dim d2);
	go_string fmt (r,sps)
	  
let write_edit ()= 
  let f = open_out edit_file in
  let fmt = formatter_of_out_channel f in
  go_string fmt (elements,!spins);
  fprintf fmt "@?";
  close_out f
    
let update_points _ ((sp1,sp2),p) = 
  let x = (sp1#value -. !xmin) *. !pic_w /. !dx in
  let y = (!ymax -. sp2#value ) *. !pic_h /. !dy in
  p#set [ `X1 (x -. 3.) ; `Y1 (y -. 3.) ;
	  `X2 (x +. 3.) ; `Y2 (y +. 3.) ];
  ()

let refresh (* canvas *) pic () = 
  eprintf "@.------------------------------refresh------------------------------@.";
  write_edit (); (* ignore (Sys.command "cp fig.edit fig.bak"); *)
  make_png ();
  let pixbuf = GdkPixbuf.from_file png_file in
  pic#set [`PIXBUF pixbuf];
  
  pic_w := float_of_int (GdkPixbuf.get_width pixbuf);
  pic_h := float_of_int (GdkPixbuf.get_height pixbuf);
  
  (*mise a l'echelle de tous les points 
    au cas ou il y aurait un changement de bbox*)
  Hashtbl.iter update_points pointstable;
  
  eprintf "refresh:@.";
  eprintf "  xmin = %f@." !xmin;
  eprintf "  xmax = %f@." !xmax;
  eprintf "  ymin = %f@." !ymin;
  eprintf "  ymax = %f@." !ymax;
  eprintf "  png  = %f x %f pixels@." !pic_w !pic_h;

  
(*   canvas#set [`WIDTH_PIXELS 50];  *)
(*   canvas#set_pixels_per_unit 2.; *)
(*   canvas#update_now (); *)
  ()
  


(*------------------------------------------------------------------------*)

let highlight_point item ev = 
  begin match ev with
  | `ENTER_NOTIFY _ ->
      item#set [ `FILL_COLOR "red" ]
  | `LEAVE_NOTIFY ev ->
      let state = GdkEvent.Crossing.state ev in
      if not (Gdk.Convert.test_modifier `BUTTON1 state)
      then item#set [`FILL_COLOR "black" ]
  | `BUTTON_PRESS ev ->
      let curs = Gdk.Cursor.create `FLEUR in
      item#grab [`POINTER_MOTION; `BUTTON_RELEASE] curs 
	(GdkEvent.Button.time ev)
  | `BUTTON_RELEASE ev ->
      item#ungrab (GdkEvent.Button.time ev)
  | _ -> ()
  end ;
  false

let move_point x y s item ev =
  begin match ev with
  | `MOTION_NOTIFY ev ->
      let state = GdkEvent.Motion.state ev in
      let x = GdkEvent.Motion.x ev  in
      let y = GdkEvent.Motion.y ev  in
      if Gdk.Convert.test_modifier `BUTTON1 state && (belong x y)
      then 
	begin
	  let (sp1,sp2),_ = Hashtbl.find pointstable s in
	    sp1#set_value (!xmin+.(x*. !dx/. !pic_w));
	    sp2#set_value (!ymax-.(y*. !dy/. !pic_h));
	    item#set [ `X1 (x -. 3.) ; `Y1 (y -. 3.) ;
		       `X2 (x +. 3.) ; `Y2 (y +. 3.) ]
	end
  | `BUTTON_RELEASE ev -> 
      eprintf "toto@.";
  | _ -> ()
  end ; false

let draw_point root s n1 n2 d1 d2 = match d1,d2 with
  |Pt,Pt -> ()
  |Bp,Bp -> 
     let x = (n1 -. !xmin) *. !pic_w /. !dx in
     let y = (!ymax -. n2) *. !pic_h /. !dy in
     let point = GnoCanvas.ellipse ~x1:(x-.3.) ~x2:(x+.3.) ~y1:(y-.3.) ~y2:(y+.3.) 
       ~props:[ `FILL_COLOR "black" ; `OUTLINE_COLOR "black" ; `WIDTH_PIXELS 0 ] root in
     let sigs = point#connect in
     sigs#event (highlight_point point) ;
     sigs#event (move_point x y s point);
     points := point::!points;
     ()
  |Cm,Cm -> ()
  |Mm,Mm -> ()
  |Inch,Inch -> ()



(*------------------------------------------------------------------------*)

let point_of_spin s sb () = 
  let n = sb#value in
  try 
    let (sb1,_),p = Hashtbl.find pointstable s in 
    if (sb == sb1)
    then begin
      let n = (n -. !xmin) *. !pic_w /. !dx in
      p#set [ `X1 (n -. 3.) ; `X2 (n +. 3.) ]
    end
    else begin
      let n = (!ymax -. n) *. !pic_h /. !dy in
      p#set [ `Y1 (n -. 3.) ; `Y2 (n +. 3.) ] 
    end
  with Not_found -> ()

let left_part_lign pic vbox vbox2 vbox3 s n d s' = 
  GMisc.label ~text:(s^s') ~packing:vbox#add ();
  GMisc.label ~text:(string_of_dim d) ~packing:vbox3#add ();      
  let sb = GEdit.spin_button ~packing:vbox2#add ~digits:2 ~numeric:true ~wrap:true ()
  in
    sb#adjustment#set_bounds ~lower:(-200.) ~upper:200. ~step_incr:0.01 ();
    sb#set_value n;
    sb#adjustment#connect#value_changed ~callback:(point_of_spin s sb (* ;refresh pic *));
    spins := sb::!spins;
    ()

let left_part pic vbox vbox2 vbox3 root = function
  | Num (s,(n,d)) ->
      left_part_lign pic vbox vbox2 vbox3 s n d " :" 
  | Point (s,(n1,d1),(n2,d2)) -> 
      left_part_lign pic vbox vbox2 vbox3 s n1 d1 " xpart :" ;
      left_part_lign pic vbox vbox2 vbox3 s n2 d2 " ypart :" ;
      draw_point root s n1 n2 d1 d2


let zoom canvas zoo = 
  z:= !z +. zoo;
  canvas#set_pixels_per_unit !z

  

(*------------------------------------------------------------------------*)


(* *)
let main () =
  let window = GWindow.window 
    ~title:"GMLPost" () in
  let vb = GPack.vbox ~spacing:10 ~packing:window#add () in
  
  (* Menu bar *)
  let menubar = GMenu.menu_bar ~packing:vb#pack () in
    
  window#connect#destroy ~callback:write_edit ;
  window#connect#destroy ~callback:Main.quit ;

  let hbox = GPack.hbox ~spacing:10 ~packing:vb#add () in
  let scrolled_window = GBin.scrolled_window  ~width:350
    ~border_width: 10 ~hpolicy: `AUTOMATIC ~packing: hbox#add ()
  in


  let hbox2 = GPack.hbox ~spacing:10 ~packing:scrolled_window#add_with_viewport () in
  let frame = GBin.frame ~shadow_type:`IN ~packing:hbox#add () in
  let canvas = GnoCanvas.canvas ~width:(int_of_float (canvas_width)) 
    ~height:(int_of_float (canvas_height)) ~packing:frame#add () in
    canvas#set_scroll_region 0. 0. canvas_width canvas_height ;
  let root = canvas#root in  

(*   let signal = canvas#connect in *)
(*   signal#event (zoom_changed canvas); *)

  let pixbuf = GdkPixbuf.from_file png_file in
  
  let pic = GnoCanvas.pixbuf root ~pixbuf in
  pic_w := float_of_int (GdkPixbuf.get_width pixbuf);
  pic_h := float_of_int (GdkPixbuf.get_height pixbuf);

  let factory = new GMenu.factory menubar in
  let accel_group = factory#accel_group in
  let file_menu = factory#add_submenu "File" in
  let zoom_menu = factory#add_submenu "Zoom" in
  
  (* File menu *)
  let factory = new GMenu.factory file_menu ~accel_group in
  factory#add_item "Refresh" ~key:_r ~callback: (refresh (* canvas *) pic);
  let factory = new GMenu.factory file_menu ~accel_group in
  factory#add_item "Quit" ~key:_Q ~callback: Main.quit;
  
  (* Zoom *)
  let factory = new GMenu.factory zoom_menu ~accel_group in
  factory#add_item "++" ~key:_A ~callback:(fun()->zoom canvas 0.1);
  factory#add_item "--" ~key:_B ~callback:(fun()->zoom canvas (-.0.1));

  window#add_accel_group accel_group;  
  
  let vbox = GPack.vbox ~spacing:10 ~packing:hbox2#add () in
  let vbox2 = GPack.vbox ~spacing:10 ~packing:hbox2#add () in
  let vbox3 = GPack.vbox ~spacing:10 ~packing:hbox2#add () in
  
  List.iter (left_part pic vbox vbox2 vbox3 root) elements;
  spins := List.rev !spins;
  points := List.rev !points;

  init_table (elements,!spins,!points);
  window#show ();
  Main.main ()

;;

main ()
