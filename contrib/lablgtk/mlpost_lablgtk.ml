
(*    Lablgtk - Examples                                                  *)

open StdLabels


let _ = GMain.Main.init ()


(*let file_dialog ~title ~callback ?filename () =
  let sel =
    GWindow.file_selection ~title ~modal:true ?filename () in
  ignore(sel#cancel_button#connect#clicked ~callback:sel#destroy);
  ignore(sel#ok_button#connect#clicked ~callback:
    begin fun () ->
      let name = sel#filename in
      sel#destroy ();
      callback name
    end);
  sel#show ()
*)

let window = GWindow.window ~width:500 ~height:300 ~title:"editor" ()
let vbox = GPack.vbox ~packing:window#add ()



let menubar = GMenu.menu_bar ~packing:vbox#pack ()

open Mlpost
let fig = ref (fun () -> Command.nop)
let set_fig = (:=) fig

let width = ref 400
let height = ref 500

let window2 = GWindow.window ~width:!width ~height:!height ~title:"view" ()
let () =
  ignore(window2#connect#destroy ~callback:GMain.quit);
  ignore(window2#show ())


let new_pixmap width height =
  let drawable = GDraw.pixmap ~width ~height () in
  drawable#set_foreground `WHITE ;
  drawable#rectangle
    ~x:0 ~y:0 ~width ~height ~filled:true () ;
  drawable

let pm = ref (new_pixmap !width !height)

let need_update = ref true

let origin = ref Point.origin

let paint () =
  try
    let w,h = (float_of_int !width,float_of_int !height) in
    let fig = (!fig ()) in
    let fig = Picture.shift !origin fig in
    let fig = Picture.shift (Point.ptp (w/.2.,h/.2.)) fig in
    let _ = Mlpost.Concrete.float_of_num (Picture.width fig) in 
    let cr = Cairo_lablgtk.create !pm#pixmap in
      !pm#rectangle ~x:0 ~y:0 
	~width:!width ~height:!height ~filled:true ();
      Cairost.emit_cairo cr (w,h) fig
  with e -> Format.eprintf "Error raised inside figure generation@ :@ %s@."
    (Printexc.to_string e)

let refresh da =
  need_update := true ;
  GtkBase.Widget.queue_draw da#as_widget

let grow_pixmap () =
  pm := new_pixmap !width !height ;
  need_update := true 
  (* no need to queue a redraw here, an expose 
     event should follow the configure, right ? *)

let config_cb ev =
  let w = GdkEvent.Configure.width ev in
  let h = GdkEvent.Configure.height ev in
  let has_grown = w > !width || h > !height in
  width := w ;
  height := h ;
  if has_grown
  then grow_pixmap () ;
  true

let expose da x y width height =
  let gwin = da#misc#window in
  let d = new GDraw.drawable gwin in
  d#put_pixmap ~x ~y ~xsrc:x ~ysrc:y ~width ~height !pm#pixmap

let expose_cb da ev =
  let area = GdkEvent.Expose.area ev in
  let module GR = Gdk.Rectangle in
    if !need_update (*&& editor#text#buffer#modified*) then paint ();
    expose da (GR.x area) (GR.y area) (GR.width area) (GR.height area);
    need_update := false;
    true
(*
let button_ev da ev =
  match GdkEvent.get_type ev with
  | `BUTTON_RELEASE -> refresh da;true
  | _ -> false

let key_press da ev =
  match GdkEvent. with
  | `TOP | `BOTTOM | `LEFT | `RIGHT as x -> 
      let dp = match x  with
	| `TOP -> Point.cmp (0.,0.5)
	| `BOTTOM -> Point.cmp (0.,-.0.5)
	| `RIGHT -> Point.cmp (0.5,0.)
	| `LEFT -> Point.cmp (-.0.5,0.) in
	origin := Point.add !origin dp;
	refresh da;true
  | _ -> false
*)
let init packing =
  let da = GMisc.drawing_area ~width:!width ~height:!height ~packing () in
    da#misc#set_can_focus true ;
    da#event#add [ `KEY_PRESS ];
    ignore (da#event#connect#expose (expose_cb da));
    ignore (da#event#connect#configure       (config_cb));
(*    ignore (da#event#connect#key_press (button_ev da))*)

    da


let dda = 
  let dda = init window2#add in
  window2#show ();
    dda

(** Editor window *)
let create_option packing ?label l =
  (match label with
    | None -> ()
    | Some text -> ignore (GMisc.label ~text ~packing ()));
  let menu = GMenu.menu () in
  let optionmenu = GMenu.option_menu ~packing () in
    optionmenu #set_menu menu;
    optionmenu #set_history 3;
    ignore (List.fold_left ~f:(fun group (s,(c:unit -> unit)) -> 
				 let c () = c ();refresh dda in
			 let menuitem = GMenu.radio_menu_item
			   ?group
			   ~label:s
			   ~packing:menu#append () in
			   ignore(menuitem#connect#toggled c); 
			   Some (menuitem#group)
		      ) ~init:None l)
      
let create_option = create_option vbox#pack

let create_text ?label get set =
  (match label with
    | None -> ()
    | Some text -> ignore (GMisc.label ~text ~packing:vbox#pack ()));
  let text = GText.view ~packing:vbox#pack ~show:true () in
    text#buffer#set_text (get ());
    ignore (text#buffer#connect#changed 
	      (fun () -> set (text#buffer#get_text ());refresh dda))



open GdkKeysyms

let main () =
  ignore(window#connect#destroy ~callback:GMain.quit);
  ignore(window#show ());
  GMain.main ()
