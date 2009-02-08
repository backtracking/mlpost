open GtkInit
open Format
open Dviinterp

module Cairo_device =
struct
  type t = { window : GWindow.window;
             pixmap : GDraw.pixmap;
             pic : Cairo.t;
             fonts :(string,Cairo_ft.font_face * Cairo_ft.ft_face) Hashtbl.t}
             
      
  let width = 450
  let height = 600
  let factor = 0.000001

  let debug = ref true
  let ft = Cairo_ft.init_freetype ()

  let find_font font_name = 
    if !debug then
      printf "Cairo : Loading font %s@." font_name;
    let filename = Dviinterp.find_file (font_name^".pfb") in
    if !debug then
      printf "Trying to find font at %s...@." filename;
    let face = Cairo_ft.new_face ft filename in
    Cairo_ft.font_face_create_for_ft_face face 0,face

  let clean_up s = 
    Hashtbl.iter (fun _ (_,x) -> Cairo_ft.done_face x) s.fonts

  let reset () = 
    if !debug then
      printf "Create the window@.";
    let w = GWindow.window ~title:"Cairo Text API" () in
    ignore (w#connect#destroy GMain.quit);
    if !debug then
      printf "Create the picture@.";
    let p = GDraw.pixmap ~width ~height ~window:w () in
    let cr = Cairo_lablgtk.create p#pixmap in
    Cairo.set_source_rgb cr 0. 0. 1. ;
    Cairo.set_line_width cr 1. ;
    Cairo.show_page cr ;
    Cairo.fill cr;
    {window = w;
     pixmap = p;
     pic = cr;
     fonts = Hashtbl.create 10}

  let fill_rect s x1 y1 x2 y2 = 
    let x1 = factor *. (Int32.to_float x1) 
    and y1 = factor *. (Int32.to_float y1)
    and x2 = factor *. (Int32.to_float x2)
    and y2 = factor *. (Int32.to_float y2) in
    if !debug then
      printf "Draw a rectangle in (%f,%f,%f,%f)@." x1 y1 x2 y2;
    Cairo.save s.pic;
    Cairo.set_source_rgb s.pic 0. 1. 0. ;
    Cairo.rectangle s.pic x1 y1 x2 y2;
    Cairo.stroke s.pic;
    Cairo.restore s.pic

  let draw_char s font char x y = 
    let f = 
      try
        fst (Hashtbl.find s.fonts font)
      with Not_found -> let f = find_font font in
      Hashtbl.add s.fonts font f;
      fst f in
    let char = Int32.to_int char 
    and x = factor *. Int32.to_float x
    and y = factor *. Int32.to_float y in
    if !debug then
      printf "Draw the char %i of %s  in (%f,%f)@." char font x y ;
    Cairo.save s.pic;
    Cairo.set_source_rgb s.pic 1. 0. 0. ;
    Cairo.set_font_face s.pic f ;
    Cairo.show_glyphs s.pic 
      [|{Cairo.index = char;
         Cairo.glyph_x = x;
         Cairo.glyph_y = y}|];
    Cairo.stroke s.pic;
    Cairo.restore s.pic

  let display s =  
    if !debug then
      printf "Display@.";
    ignore (GMisc.pixmap s.pixmap ~packing:s.window#add ());
    s.window#show () ;
    GMain.main ();
    clean_up s

end

module Cairo_interp = Interp(Cairo_device)
  
let _ =
  Cairo_interp.set_debug false;
  match Array.length Sys.argv with
    | 1 ->
	printf "Usage : dviinterp <file1.dvi> <file2.dvi> ...\n"
    | n ->
	for i = 1 to n-1 do
	  let s = Sys.argv.(i) in
          List.iter Cairo_device.display (Cairo_interp.load_file s);
	done
