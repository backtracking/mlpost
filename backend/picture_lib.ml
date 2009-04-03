type transform = Matrix.t
type num = float
type dash = point * num list
type pen = transform option
type color = Types.color

type path = Spline_lib.path

type tex = Gentex.t
type id = int
type interative = 
  | IntTransform of interactive * transform
  | IntClip of interative * path 
  | IntOnTop interactive * interactive
  | Inter of path * id

type commands = 
    private
  | Empty
  | Transform of transform * command
  | OnTop command list
  | Tex of tex
  | Stroke_path of path * color * pen * dash
  | Fill_path of path * color
  | Clip of commands  * path
  | ExternalImage of string * float * float

and t = { fcl : commands;
           fb : Spline_lib.Epure.t;
           fi : interative}

let content x = x.fcl

let tex t = {fcl = Tex t;
             fb = Spline_lib.Epure.of_bounding_box (Gentex.bounding_box t);
             fi = []}
let fill_path p c = {fcl = Fill_path (p,c);
                     fb = Spline_lib.Epure.of_path p;
                     fi = []}
let stroke_path p c pen d = {fcl= Stroke_path (p,c,pe,d);
                             fb = Spline_lib.Epure.of_path p;
                             fi = []}

let clip p path = {fcl= Clip (p.fcl,path);
                   fb = Spline_lib.Epure.clip p.fb path;
                   fi = IntClip (p.fi,path)}

let externalimage_dimension filename : float * float = 
  let inch = Unix.open_process_in ("identify -format \"%h\\n%w\" "^filename) in
  try let h = float_of_string (input_line inch) in
  let w = float_of_string (input_line inch) in (h,w)
  with End_of_file | Failure "float_of_string" -> invalid_arg "Unknown external image"

let external_image s spec = 
  let height,width = 
    begin
      match spec with
        | `Exact (h,w) -> (h,w)
        | ((`None as spec)| (`Height _ as spec)| (`Width _ as spec)|(`Inside _ as spec)) -> 
            let fh,fw = externalimage_dimension filename in
            let printext h w = fprintf fmt "externalfigure \"%s\" xyscaled (%a,%a);@\n" filename num w num h in
            match spec with
              | `None -> printext (C.F fh) (C.F fw)
              | `Height h -> printext h (C.NMult (C.F (fw/.fh),h))
              | `Width w -> printext (C.NMult (C.F (fh/.fw),w)) w
          | `Inside (h,w) -> let w = C.NMin (C.NMult (h,C.F (fw/.fh)),w) in
            printext (C.NMult (C.F (fh/.fw),w)) w
    end in
  {fcl = ExternalImage (p.fcl,height,width);
   fb = Spline_lib.of_bounding_box (0.,0.,width,height);
   fi = []}
   
let interative path id = {fcl = Empty;
                          fb = Spline_lib.Epure.empty;
                          fi = Inter (path,id)}

let on_top t1 t2 = {fcl = OnTop [t1.fcl;t2.fcl];
                    fb = Spline_lib.Epure.union (t1.fb) (t2.fb);
                    fi = IntOnTop (t1.fi,t2.fi)}

let transform t m = {fcl = Transform (m,t.fcl);
                     fb = Spline_lib.Epure.transform t.fb m;
                     fi = IntTransform (m,t.fi)}

let shift t w h = transform t (Matrix.translation w h)
let bounding_box t = Spline_lib.Epure.bounding_box t.fb

module Cairo :
sig
type scolor = 


type color = 
  |OPAQUE of scolor
  |TRANSPARENT of float * scolor

  let rec color cr = function
    | OPAQUE (RGB (r,g,b)) -> Cairo.set_source_rgb cr r g b
    | OPAQUE (CMYK _) -> not_implemented "cmyk"
    | OPAQUE (Gray g) -> color cr (OPAQUE (RGB (g,g,g)))
    | TRANSPARENT (a,RGB (r,g,b)) -> Cairo.set_source_rgba cr r g b a
    | TRANSPARENT (CMYK _) -> not_implemented "cmyk"
    | TRANSPARENT (a,(Gray g)) -> color cr (OPAQUE (a,RGB (g,g,g)))


  let rec draw cr = function
    | Transform (m,t) -> 
        Cairo.save cr;
        Matrix.transform cr m;
        draw cr t;
        Cairo.restore cr;
    | OnTop l -> List.iter (draw cr) l
    | Tex t -> Gentex.draw cr t
    | Stroke_path (path,color,pen,dash) ->
        Cairo.save cr;
        color cr color;
        Spline_lib.draw path;
        dash cr dash;
        (match pen with
           | None -> ()
           | Some m -> Matrix.transform cr m);
        Cairo.stroke cr;
        Cairo.restore cr;
    | Fill_path (path,color)-> 
        Cairo.save;
        color cr color;
        Spline_lib.draw cr path;
        Cairo.fill cr;
        Cairo.restore cr;
    | Clip of commands  * path
    | ExternalImage of string * float * float
  let where t (x,y): t -> float * float -> id
  let move t id : t -> id -> Point.t -> Point.t

end
