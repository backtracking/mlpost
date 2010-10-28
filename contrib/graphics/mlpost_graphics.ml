open Mlpost

module Virtual = struct

  let num i = Num.px (float i)

  type t = {mutable x   : int;
            mutable y   : int;
            mutable cmds  : Command.t list;
            mutable color : Color.t;
            mutable filename : string}

  let t = {x = 0; y = 0; cmds = [];
           color = Color.black;
           filename="mlpost_graphics"}

  let clear_graph () = t.x <- 0; t.y <- 0; t.cmds <- []; t.color <- Color.black

  let get_cmds () = Command.seq t.cmds

  let set_color c = t.color <- c
  let red = Color.red
  let blue = Color.blue
  let green = Color.green
  let black = Color.black
  let white = Color.white

  let moveto x y = t.x <- x; t.y <- y

  let push c = t.cmds <- c::t.cmds

  let circle x y r =
    let circle = Path.scale (num r) Path.fullcircle in
    let circle = Path.shift (Point.pt (num x,num y)) circle in
    circle

  let fill_circle x y r =
    push (Path.fill ~color:t.color (circle x y r))

  let lineto x y =
    push (Path.draw (Path.pathn [num t.x, num t.y; num x, num y]))

  let rect x y w h =
    let rect = Path.unitsquare in
    (* let rect = Path.shift (Point.pt (Num.bp 0.5,Num.bp 0.5)) rect in *)
    let rect = Path.yscale (num h) rect in
    let rect = Path.xscale (num w) rect in
    let rect = Path.shift (Point.pt (num x,num y)) rect in
    rect

  let draw_rect x y w h =
    push (Path.draw (rect x y w h))

  let set_emit f = t.filename <- f

  let synchronize =
    at_exit (fun () -> Printf.printf "Dump!!\n%!";
      Mps.dump ();Cairost.dump_png ());
    let c = ref (-1) in
    fun () -> incr c;
      Metapost.emit (t.filename^(string_of_int !c)) (get_cmds ())

  let draw_string s =
    let pic = Picture.tex s in (* TODO escape character *)
    let pic = Picture.shift (Point.pt (num t.x,num t.y)) pic in
    push pic

end

(* Use also Graphics *)
module Double = struct
  include Graphics
  let clear_graph () = Virtual.clear_graph (); Graphics.clear_graph ()

  let get_cmds = Virtual.get_cmds
  let set_ratio = Virtual.set_ratio

  let set_color (c1,c2) = Virtual.set_color c1; Graphics.set_color c2
  let red = (Virtual.red,Graphics.red)
  let green = (Virtual.green,Graphics.green)
  let blue = (Virtual.blue,Graphics.blue)
  let black = (Virtual.black,Graphics.black)
  let white = (Virtual.white,Graphics.white)

  let moveto x y = Virtual.moveto x y; Graphics.moveto x y

  let fill_circle x y r = Virtual.fill_circle x y r;Graphics.fill_circle x y r

  let lineto x y = Virtual.lineto x y; Graphics.lineto x y

  let draw_rect x y w h = Virtual.draw_rect x y w h; Graphics.draw_rect x y w h

  let set_emit = Virtual.set_emit

  let synchronize () = Virtual.synchronize (); Graphics.synchronize ()

  let draw_string s = Virtual.draw_string s;Graphics.draw_string s

end
