open GMain
open GdkKeysyms
open Glexer

let () = ignore (GtkMain.Main.init ())

let file = Sys.argv.(1)

let elements = Glexer.read_file file
let () = Format.printf "%d elements@." (List.length elements)


let rec fonction vbox vbox2 = function
  | Num (s,(n,d)) -> GMisc.label ~text:s ~packing:vbox#add ();
      let sb = GEdit.spin_button ~packing:vbox2#add ~digits:2 ~numeric:true ~wrap:true ()
      in
      sb#adjustment#set_bounds ~lower:min_float ~upper:max_float ~step_incr:0.01 ();
      sb#set_value n;
      sb#connect#wrapped (fun () -> prerr_endline "Wrapped!");
      ()
  | Point (s,(n1,d1),(n2,d2)) -> ()

(* *)
let main () =
  let window = GWindow.window 
    ~title:"Simple lablgtk program" () in
  let hbox = GPack.hbox ~spacing:10 ~packing:window#add () in
  window#connect#destroy ~callback:Main.quit;

  let scrolled_window = GBin.scrolled_window  ~width:250 ~height:100
    ~border_width: 10 ~hpolicy: `AUTOMATIC ~packing: hbox#add ()
  in
  let hbox2 = GPack.hbox ~spacing:10 ~packing:scrolled_window#add_with_viewport () in
  let vbox = GPack.vbox ~spacing:10 ~packing:hbox2#add () in
  let vbox2 = GPack.vbox ~spacing:10 ~packing:hbox2#add () in
  
  List.iter (fonction vbox vbox2) elements;
  (* let sb = *)
(*     GEdit.spin_button ~packing:vbox2#add ~digits:0 ~numeric:true ~wrap:true () *)
(*   in *)
(*   let sb2 = *)
(*     GEdit.spin_button ~packing:vbox2#add ~digits:0 ~numeric:true ~wrap:true () *)
(*   in *)
(*   GMisc.label ~text:"totooo" (\* ~xalign:0.02 *\) ~packing:vbox#add (); *)
(*   GMisc.label ~text:"t"  ~packing:vbox#add (); *)
  
    
  
(*   sb#adjustment#set_bounds ~lower:0. ~upper:10.0 ~step_incr:1. (); *)
(*   sb#set_value 0.; *)
(*   sb#connect#wrapped (fun () -> prerr_endline "Wrapped!"); *)
  
 
(*   sb2#adjustment#set_bounds ~lower:0. ~upper:10.0 ~step_incr:1. (); *)
(*   sb2#set_value 0.; *)
(*   sb2#connect#wrapped (fun () -> prerr_endline "Wrapped!"); *)

  (* (\* Menu bar *\) *)
(*   let menubar = GMenu.menu_bar ~packing:vbox#pack () in *)
(*   let factory = new GMenu.factory menubar in *)
(*   let accel_group = factory#accel_group in *)
(*   let file_menu = factory#add_submenu "File" in *)

(*   (\* File menu *\) *)
(*   let factory = new GMenu.factory file_menu ~accel_group in *)
(*   factory#add_item "Quit" ~key:_Q ~callback: Main.quit; *)

(*   (\* Button. *\) *)
(*   let button = GButton.button ~label:"Push me!" *)
(*       ~packing:vbox#add () in *)
(*   button#connect#clicked ~callback: (fun () -> prerr_endline "Ouch!"); *)

(*   (\* Display the windows and enter Gtk+ main loop *\) *)
(*   window#add_accel_group accel_group; *)
  window#show ();
  Main.main ()
;;

main ()
