open Types
open Point_lib
exception Not_implemented of string

let not_implemented s = raise (Not_implemented s)

type transform = Matrix.t
type num = float
type point = Point_lib.point
type dash = float * num list
type pen = transform
type color = Types.color

type path = Spline_lib.path

type tex = Gentex.t
type id = int
type interactive = 
  | IntEmpty
  | IntTransform of interactive * transform
  | IntClip of interactive * path 
  | IntOnTop of interactive * interactive
  | Inter of path * id

type commands = 
  | Empty
  | Transform of transform * commands
  | OnTop of commands list
  | Tex of tex
  | Stroke_path of path * color option * pen option * dash option
  | Fill_path of path * color option
  | Clip of commands  * path
  | ExternalImage of string * float * float

and t = { fcl : commands;
           fb : Spline_lib.Epure.t;
           fi : interactive}

let content x = x.fcl

let empty = { fcl = Empty; fb = Spline_lib.Epure.empty ; fi = IntEmpty }
let tex t = {fcl = Tex t;
             fb = Spline_lib.Epure.of_bounding_box (Gentex.bounding_box t);
             fi = IntEmpty}
let fill_path p c = {fcl = Fill_path (p,c);
                     fb = Spline_lib.Epure.of_path p;
                     fi = IntEmpty}
let stroke_path p c pen d = {fcl= Stroke_path (p,c,pen,d);
                             fb = Spline_lib.Epure.of_path p;
                             fi = IntEmpty}

let draw_point p = stroke_path (Spline_lib.create p p p p) None None None

let clip p path = {fcl= Clip (p.fcl,path);
                   fb = Spline_lib.Epure.of_path path; 
(* la bounding box d'un clip est la bounding_box du chemin fermé*)
                   fi = IntClip (p.fi,path)}

let externalimage_dimension filename : float * float = 
  let inch = Unix.open_process_in ("identify -format \"%h\\n%w\" "^filename) in
  try let h = float_of_string (input_line inch) in
  let w = float_of_string (input_line inch) in (h,w)
  with End_of_file | Failure "float_of_string" -> invalid_arg "Unknown external image"

let external_image filename spec = 
  let height,width = 
    begin
      match spec with
        | `Exact (h,w) -> (h,w)
        | ((`None as spec)| (`Height _ as spec)| (`Width _ as spec)|(`Inside _ as spec)) -> 
            let fh,fw = externalimage_dimension filename in
            match spec with
              | `None -> fh,fw
              | `Height h -> h,(fw/.fh)*.h
              | `Width w -> (fh/.fw)*.w,w
          | `Inside (h,w) -> 
              let w = min (h*.(fw/.fh)) w in
              (fh/.fw)*.w,w
    end in
  {fcl = ExternalImage (filename,height,width);
   fb = Spline_lib.Epure.of_bounding_box ({x=0.;y=0.},{x=width;y=height});
   fi = IntEmpty}
   
let interative path id = {fcl = Empty;
                          fb = Spline_lib.Epure.empty;
                          fi = Inter (path,id)}

let on_top t1 t2 = {fcl = OnTop [t1.fcl;t2.fcl];
                    fb = Spline_lib.Epure.union (t1.fb) (t2.fb);
                    fi = IntOnTop (t1.fi,t2.fi)}

let transform m t = {fcl = Transform (m,t.fcl);
                     fb = Spline_lib.Epure.transform m t.fb;
                     fi = IntTransform (t.fi,m)}

let shift t w h = transform (Matrix.xy_translation w h) t
let bounding_box t = Spline_lib.Epure.bounding_box t.fb

module ToCairo =
struct
  let rec color cr = function
    | OPAQUE (RGB (r,g,b)) -> Cairo.set_source_rgb cr r g b
    | OPAQUE (CMYK _) -> not_implemented "cmyk"
    | OPAQUE (Gray g) -> color cr (OPAQUE (RGB (g,g,g)))
    | TRANSPARENT (a,RGB (r,g,b)) -> Cairo.set_source_rgba cr r g b a
    | TRANSPARENT (a,CMYK _) -> not_implemented "cmyk"
    | TRANSPARENT (a,(Gray g)) -> color cr (TRANSPARENT (a,RGB (g,g,g)))

  let color_option cr = function
    | None -> ()
    | Some c -> color cr c

  let dash cr = function
    | None | Some (_,[]) -> ();
    | Some (f,l) -> Cairo.set_dash cr (Array.of_list l) f

  let inversey cr height = 
    Cairo.translate cr ~tx:0. ~ty:height;
    Cairo.scale cr ~sx:1. ~sy:(-.1.)

  let rec draw_aux cr = function
    | Empty -> ()
    | Transform (m,t) -> 
        Cairo.save cr;
        Matrix.transform cr m;
        (*Format.printf "Transform : %a@." Matrix.print m;*)
        draw_aux cr t;
        Cairo.restore cr
    | OnTop l -> List.iter (draw_aux cr) l
    | Tex t -> 
        Cairo.save cr;
        let ({y=min},{y=max}) = Gentex.bounding_box t in
        inversey cr (max+.min);
        Gentex.draw cr t;
        Cairo.restore cr
    | Stroke_path (path,c,pen,d) ->
        Cairo.save cr;
        color_option cr c;
        Spline_lib.ToCairo.draw cr path;
        dash cr d;
        (match pen with
           | None -> ()
           | Some m -> Matrix.transform cr m);
        Cairo.stroke cr;
        Cairo.restore cr
    | Fill_path (path,c)-> 
        Cairo.save cr;
        color_option cr c;
        Spline_lib.ToCairo.draw cr path;
        Cairo.fill cr;
        Cairo.restore cr
    | Clip (com,p) -> 
        Cairo.save cr;
        Spline_lib.ToCairo.draw cr p;
        Cairo.clip cr;
        draw_aux cr com;
        Cairo.restore cr
    | ExternalImage (filename,height,width) -> 
        Cairo.save cr;
        inversey cr height;
        let img = Cairo_png.image_surface_create_from_file filename in
        let iwidth = float_of_int (Cairo.image_surface_get_width img) in
        let iheight = float_of_int (Cairo.image_surface_get_height img) in
        Cairo.scale cr (width/.iwidth) (height/.iheight);
        Cairo.set_source_surface cr img 0. 0.;
        Cairo.paint cr;
        Cairo.restore cr

  let draw cr width height p =
    Cairo.save cr;
    inversey cr height;
    Cairo.set_line_width cr 1.;
    draw_aux cr p.fcl;
    (*Spline_lib.Epure.draw cr p.fb;*)
    Cairo.restore cr

  let where cr t (x,y) = not_implemented "where"
  let move t id p = not_implemented "move"

end


module Dash = 
struct
  type t = float * float list

  type input_dash = 
    | On of float
    | Off of float

  let shifted f (x,d) = (x+.f,d)

  let line = 0., [3.; 3. ]
  let dots = 0., [0.; 5.]

  let rec on acc = function
    | [] -> [acc]
    | On f::l -> on (f +. acc) l
    | Off f::l -> acc::(off f l)
  and off acc = function
    | [] -> [acc]
    | On f::l -> acc::(on f l)
    | Off f::l -> off (f +. acc) l
  and to_dash = function
    | [] -> []
    | On f::l -> on f l
    | Off f::l -> 0. :: (off f l)

  let pattern l = 
    0., to_dash l

  let scale f (x,l) = 
    x, List.map (fun z -> f *. z) l
end
