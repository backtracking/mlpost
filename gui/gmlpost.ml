open GMain
open GdkKeysyms
open Glexer
open Format

let () = ignore (GtkMain.Main.init ())

let file = Sys.argv.(1)

let elements = Glexer.read_file file
let () = Format.printf "%d elements@." (List.length elements)

let spins = ref []

let size = ref (200.,200.)

let belong x y = 
  let x',y' = !size in
  if(x>0. && x<x' && y>0. && y<y') then true
  else false

let string_of_dim = function
  |Pt -> "pt"
  |Bp -> "bp"
  |Cm -> "cm"
  |Mm -> "mm"
  |Inch -> "inch"

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

let move_point x y item ev =
  begin match ev with
  | `MOTION_NOTIFY ev ->
      let state = GdkEvent.Motion.state ev in
      let x = GdkEvent.Motion.x ev +. x in
      let y = GdkEvent.Motion.y ev +. y in
      if Gdk.Convert.test_modifier `BUTTON1 state && (belong x y)
      then begin
	item#set [ `X1 (x -. 3.) ; `Y1 (y -. 3.) ;
		   `X2 (x +. 3.) ; `Y2 (y +. 3.) ; ]
      end
  | _ -> ()
  end ; false

let draw_point root s n1 n2 d1 d2 = match d1,d2 with
  |Pt,Pt -> ()
  |Bp,Bp -> 
     let w,h = !size in
     let x = n1 *. 500. /. w in
     let y = n2 *. 500. /. h in
     let point = GnoCanvas.ellipse ~x1:(x-.3.) ~x2:(x+.3.) ~y1:(y-.3.) ~y2:(y+.3.) 
       ~props:[ `FILL_COLOR "black" ; `OUTLINE_COLOR "black" ; `WIDTH_PIXELS 0 ] root in
     let sigs = point#connect in
     sigs#event (highlight_point point) ;
     sigs#event (move_point x y point);
     ()
  |Cm,Cm -> ()
  |Mm,Mm -> ()
  |Inch,Inch -> ()

let left_part_lign vbox vbox2 vbox3 s n d s' =
  GMisc.label ~text:(s^s') ~packing:vbox#add ();
  GMisc.label ~text:(string_of_dim d) ~packing:vbox3#add ();      
  let sb = GEdit.spin_button ~packing:vbox2#add ~digits:2 ~numeric:true ~wrap:true ()
  in
    sb#adjustment#set_bounds ~lower:(-200.) ~upper:200. ~step_incr:0.01 ();
    sb#set_value n;
    spins := sb::!spins;
    ()

let left_part vbox vbox2 vbox3 root = function
  | Num (s,(n,d)) ->
      let s = String.sub s 1 ((String.length s )-2) in 
      left_part_lign vbox vbox2 vbox3 s n d " :" 
  | Point (s,(n1,d1),(n2,d2)) -> 
      let s = String.sub s 1 ((String.length s )-2) in
      left_part_lign vbox vbox2 vbox3 s n1 d1 " xpart :" ;
      left_part_lign vbox vbox2 vbox3 s n2 d2 " ypart :" ;
      draw_point root s n1 n2 d1 d2
  
let rec go_string fmt = function
  |[],_|_,[] -> ()
  | Num (s,(_,d))::r,sp::sps ->
      let n = sp#value in
	fprintf fmt "num %s %f %s \n" s n (string_of_dim d);
	go_string fmt (r,sps)
  | Point (s,(_,d1),(_,d2))::r,sp1::sp2::sps ->
      let n1 = sp1#value in
      let n2 = sp2#value in
	fprintf fmt "point %s %f %s , %f %s \n" s n1 (string_of_dim d1) n2 (string_of_dim d2);
	go_string fmt (r,sps)
	  
let quit ()= 
  let f = open_out "result" in
  let fmt = formatter_of_out_channel f in
    go_string fmt (elements,!spins);
    fprintf fmt "@?";
    close_out f

(* *)
let main () =
  let window = GWindow.window 
    ~title:"GMLPost" () in
  let vb = GPack.vbox ~spacing:10 ~packing:window#add () in
  
  (* Menu bar *)
  let menubar = GMenu.menu_bar ~packing:vb#pack () in
  let factory = new GMenu.factory menubar in
  let accel_group = factory#accel_group in
  let file_menu = factory#add_submenu "File" in

  (* File menu *)
  let factory = new GMenu.factory file_menu ~accel_group in
  factory#add_item "Quit" ~key:_Q ~callback: Main.quit;
    
  window#connect#destroy ~callback:quit ;
  window#connect#destroy ~callback:Main.quit ;
  window#add_accel_group accel_group;

  let hbox = GPack.hbox ~spacing:10 ~packing:vb#add () in
  let scrolled_window = GBin.scrolled_window  ~width:350
    ~border_width: 10 ~hpolicy: `AUTOMATIC ~packing: hbox#add ()
  in
  (* let button = GButton.button ~use_mnemonic:true ~label:"_Coucou" ~packing:(hbox#pack ~padding:5) () in *)
(*     button#connect#clicked ~callback:quit; *)
  let hbox2 = GPack.hbox ~spacing:10 ~packing:scrolled_window#add_with_viewport () in
  let frame = GBin.frame ~shadow_type:`IN ~packing:hbox#add () in
  let canvas = GnoCanvas.canvas ~width:500 ~height:500 ~packing:frame#add () in
  let root = canvas#root in
  root#set [`X (-200.); `Y (-200.)];
  let vbox = GPack.vbox ~spacing:10 ~packing:hbox2#add () in
  let vbox2 = GPack.vbox ~spacing:10 ~packing:hbox2#add () in
  let vbox3 = GPack.vbox ~spacing:10 ~packing:hbox2#add () in
  
  List.iter (left_part vbox vbox2 vbox3 root) elements;
  Format.printf "%d spins@." (List.length !spins);
  spins := List.rev !spins;
  Format.printf "%d spins@." (List.length !spins);
    
  window#show ();
  Main.main ()

;;

main ()
