open GMain
open GdkKeysyms
open Glexer

let () = ignore (GtkMain.Main.init ())

let file = Sys.argv.(1)

let elements = Glexer.read_file file
let () = Format.printf "%d elements@." (List.length elements)


let string_of_dim = function
  |Pt -> "pt"
  |Bp -> "bp"
  |Cm -> "cm"
  |Mm -> "mm"
  |Inch -> "inch"

let left_part_lign vbox vbox2 vbox3 s n d s' =
  GMisc.label ~text:(s^s') ~packing:vbox#add ();
  GMisc.label ~text:(string_of_dim d) ~packing:vbox3#add ();      
  let sb = GEdit.spin_button ~packing:vbox2#add ~digits:2 ~numeric:true ~wrap:true ()
  in
  sb#adjustment#set_bounds ~lower:(-200.) ~upper:200. ~step_incr:0.01 ();
  sb#set_value n;
  ()

let left_part vbox vbox2 vbox3 = function
  | Num (s,(n,d)) ->
      let s = String.sub s 1 ((String.length s )-2) in 
      left_part_lign vbox vbox2 vbox3 s n d " :" 
  | Point (s,(n1,d1),(n2,d2)) -> 
      let s = String.sub s 1 ((String.length s )-2) in
      left_part_lign vbox vbox2 vbox3 s n1 d1 " xpart :" ;
      left_part_lign vbox vbox2 vbox3 s n2 d2 " ypart :" 

(* *)
let main () =
  let window = GWindow.window ~width:800 ~height:400
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
  window#connect#destroy ~callback:Main.quit;
  window#add_accel_group accel_group;

  let hbox = GPack.hbox ~spacing:10 ~packing:vb#add () in
  let scrolled_window = GBin.scrolled_window  ~width:350
    ~border_width: 10 ~hpolicy: `AUTOMATIC ~packing: hbox#add ()
  in
  let hbox2 = GPack.hbox ~spacing:10 ~packing:scrolled_window#add_with_viewport () in
  let frame = GBin.frame ~shadow_type:`IN ~packing:hbox#add () in
  let canvas = GnoCanvas.canvas ~width:500 ~height:500 ~packing:frame#add () in
  let vbox = GPack.vbox ~spacing:10 ~packing:hbox2#add () in
  let vbox2 = GPack.vbox ~spacing:10 ~packing:hbox2#add () in
  let vbox3 = GPack.vbox ~spacing:10 ~packing:hbox2#add () in
  
  List.iter (left_part vbox vbox2 vbox3) elements;
  
  window#show ();
  Main.main ()
;;

main ()
