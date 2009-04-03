open Types

exception Not_implemented of string

let not_implemented s = raise (Not_implemented s)

type transform = Matrix.t
type num = float
type point = Point.point
type dash = point * num list
type pen = transform option
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
  | Stroke_path of path * color * pen * dash
  | Fill_path of path * color
  | Clip of commands  * path
  | ExternalImage of string * float * float

and t = { fcl : commands;
           fb : Spline_lib.Epure.t;
           fi : interactive}

let content x = x.fcl

let tex t = {fcl = Tex t;
             fb = Spline_lib.Epure.of_bounding_box (Gentex.bounding_box t);
             fi = IntEmpty}
let fill_path p c = {fcl = Fill_path (p,c);
                     fb = Spline_lib.Epure.of_path p;
                     fi = IntEmpty}
let stroke_path p c pen d = {fcl= Stroke_path (p,c,pen,d);
                             fb = Spline_lib.Epure.of_path p;
                             fi = IntEmpty}

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
   fb = Spline_lib.Epure.of_bounding_box (0.,0.,width,height);
   fi = IntEmpty}
   
let interative path id = {fcl = Empty;
                          fb = Spline_lib.Epure.empty;
                          fi = Inter (path,id)}

let on_top t1 t2 = {fcl = OnTop [t1.fcl;t2.fcl];
                    fb = Spline_lib.Epure.union (t1.fb) (t2.fb);
                    fi = IntOnTop (t1.fi,t2.fi)}

let transform t m = {fcl = Transform (m,t.fcl);
                     fb = Spline_lib.Epure.transform t.fb m;
                     fi = IntTransform (t.fi,m)}

let shift t w h = transform t (Matrix.translation w h)
let bounding_box t = Spline_lib.Epure.bounding_box t.fb

module Cairo =
struct
  let rec color cr = function
    | OPAQUE (RGB (r,g,b)) -> Cairo.set_source_rgb cr r g b
    | OPAQUE (CMYK _) -> not_implemented "cmyk"
    | OPAQUE (Gray g) -> color cr (OPAQUE (RGB (g,g,g)))
    | TRANSPARENT (a,RGB (r,g,b)) -> Cairo.set_source_rgba cr r g b a
    | TRANSPARENT (a,CMYK _) -> not_implemented "cmyk"
    | TRANSPARENT (a,(Gray g)) -> color cr (TRANSPARENT (a,RGB (g,g,g)))

  let dash cr = function
    | _,[] -> ();
    | _ -> not_implemented "dash"

  let rec draw_aux cr = function
    | Empty -> ()
    | Transform (m,t) -> 
        Cairo.save cr;
        Matrix.transform cr m;
        draw_aux cr t;
        Cairo.restore cr
    | OnTop l -> List.iter (draw_aux cr) l
    | Tex t -> Gentex.draw cr t
    | Stroke_path (path,c,pen,d) ->
        Cairo.save cr;
        color cr c;
        Spline_lib.Cairo.draw cr path;
        dash cr d;
        (match pen with
           | None -> ()
           | Some m -> Matrix.transform cr m);
        Cairo.stroke cr;
        Cairo.restore cr
    | Fill_path (path,c)-> 
        Cairo.save cr;
        color cr c;
        Spline_lib.Cairo.draw cr path;
        Cairo.fill cr;
        Cairo.restore cr
    | Clip (com,p) -> 
        Cairo.save cr;
        Spline_lib.Cairo.draw cr p;
        Cairo.clip cr;
        draw_aux cr com;
        Cairo.restore cr
    | ExternalImage (filename,height,width) -> 
        Cairo.save cr;
        let img = Cairo_png.image_surface_create_from_file filename in
        let iwidth = float_of_int (Cairo.image_surface_get_width img) in
        let iheight = float_of_int (Cairo.image_surface_get_height img) in
        Cairo.scale cr (width/.iwidth) (height/.iheight);
        Cairo.set_source_surface cr img 0. 0.;
        Cairo.paint cr;
        Cairo.restore cr

  let draw cr p = draw_aux cr p.fcl

  let where cr t (x,y) = not_implemented "where"
  let move t id p = not_implemented "move"

end
