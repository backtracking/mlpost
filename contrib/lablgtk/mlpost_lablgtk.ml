
(*    Lablgtk - Examples                                                  *)

open StdLabels
open Mlpost

open Format

class mlpost_fig ?width ?height ?packing ?show fig =

  (* Create the drawing area. *)
  let da = GMisc.drawing_area ?width ?height ?packing ?show () in
  let drawable = lazy (new GDraw.drawable da#misc#window) in


  let new_pixmap width height =
    let drawable = GDraw.pixmap ~width ~height () in
    drawable#set_foreground `WHITE ;
    drawable in

  object (self)
    inherit GObj.widget da#as_widget

    val mutable need_update = true

    (* The mlpost fig. *)
    val mutable fig = fig
    method set_fig t = fig <- t; need_update <- true
    method fig = fig

    val mutable size = (0,0)
    val mutable pm = new_pixmap 1 1
    val origin = Point.origin

    method private repaint () =
      try
        let drawable = Lazy.force drawable in
        let (width, height) as ssize = drawable#size in
        if ssize <> size then
          size <- ssize;
          pm <- new_pixmap width height;
        (* reset the pixmap *)
        pm#rectangle ~x:0 ~y:0 ~width ~height ~filled:true ();
        let w,h = (float_of_int width,float_of_int height) in
        (* *)
        let fig = Picture.shift origin fig in
        let fig = Picture.shift (Point.ptp (w/.2.,h/.2.)) fig in
        let cr = Cairo_lablgtk.create pm#pixmap in       
        Cairost.emit_cairo cr (w,h) fig;
        need_update<-false;
      with e -> Format.eprintf "Error raised inside figure generation@ :@ %s@."
        (Printexc.to_string e)
     
    (* Repaint the widget. *)
    method private expose ev =
      if need_update then self#repaint ();
      let area = GdkEvent.Expose.area ev in
      let gwin = da#misc#window in
      let d = new GDraw.drawable gwin in
      let x = Gdk.Rectangle.x area and y = Gdk.Rectangle.y area in
      let width = Gdk.Rectangle.width area 
      and height = Gdk.Rectangle.height area in
      d#put_pixmap 
        ~x ~y ~xsrc:x ~ysrc:y ~width ~height pm#pixmap

    initializer
      ignore (da#event#connect#expose
              ~callback:(fun ev -> self#expose ev; false));
      ignore (da#event#connect#configure
                ~callback:(fun _ -> need_update <- true; false));

  end


module Interface =
struct
  type interface = {
    window : GWindow.window;
    main_vbox : GPack.box;
    mutable show : bool; (* The main window is shown *)
    mutable figda : ((unit -> Command.t) * (mlpost_fig * GWindow.window)) list}

  let new_interface ?width ?height ?title () =
    let window = GWindow.window ?width ?height ?title () in
    let vbox = GPack.vbox ~packing:window#add () in
    let _ = GMenu.menu_bar ~packing:vbox#pack () in
    ignore(window#connect#destroy ~callback:GMain.quit);
    {window = window;main_vbox = vbox; show = false; figda = []}

  let remove_fig window fig =
    window.figda <- List.remove_assq fig window.figda
 
  let add_fig w ?width ?height ?title fig =
    let window = GWindow.window ?width ?height ?title () in
    let mlpost_fig = new mlpost_fig ?width ?height 
      ~packing:window#add (fig ()) in
    w.figda <- (fig,(mlpost_fig,window))::w.figda;
    ignore(window#connect#destroy 
             ~callback:(fun () -> remove_fig w fig));
    if w.show then ignore(window#show ())


  let refresh w =
    List.iter (fun (fig,(mlfig,_)) ->
                 mlfig#set_fig (fig ());
                 GtkBase.Widget.queue_draw mlfig#as_widget) w.figda


  (** Editor window *)
  let create_option w ~packing ?label l =
    (match label with
       | None -> ()
       | Some text -> ignore (GMisc.label ~text ~packing ()));
    let menu = GMenu.menu () in
    let optionmenu = GMenu.option_menu ~packing () in
    optionmenu #set_menu menu;
    optionmenu #set_history 3;
    ignore (List.fold_left ~f:(fun group (s,(c:unit -> unit)) -> 
				 let c () = c ();refresh w in
			         let menuitem = GMenu.radio_menu_item
			           ?group
			           ~label:s
			           ~packing:menu#append () in
			         ignore(menuitem#connect#toggled c); 
			         Some (menuitem#group)
		              ) ~init:None l)
      
  let create_option w = create_option w ~packing:w.main_vbox#pack
    
  let create_text w ?label first set =
    (match label with
       | None -> ()
       | Some text -> ignore (GMisc.label ~text ~packing:w.main_vbox#pack ()));
    let text = GText.view ~packing:w.main_vbox#pack ~show:true () in
    text#buffer#set_text first;
    ignore (text#buffer#connect#changed 
	      (fun () -> set (text#buffer#get_text ());refresh w))


  let main w =
    ignore(w.window#show ());
    List.iter (fun (_,(_,window)) -> ignore(window#show ())) w.figda;
    GMain.main ()

end
