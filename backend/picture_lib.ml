(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Johannes Kanig, Stephane Lescuyer                       *)
(*  Jean-Christophe Filliatre, Romain Bardou and Francois Bobot           *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

let diameter_of_a_dot = 3.
let default_line_size = 1.

module MP = Metapath_lib
open Types
open Point_lib
type transform = Matrix.t
type num = float
type point = Point_lib.t
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
  | Stroke_path of path * color option * pen * dash option
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

let ecart_of_pen pen = 
  Spline_lib.transform pen (MP.Approx.fullcircle default_line_size)

let stroke_path p c pen d = {fcl= Stroke_path (p,c,pen,d);
                             fb = Spline_lib.Epure.of_path ~ecart:(ecart_of_pen pen) p;
                             fi = IntEmpty}

let draw_point p = stroke_path (Spline_lib.create_point p) None (Matrix.scale diameter_of_a_dot) None

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

let baseline p = match p.fcl with
  | Tex tex -> Gentex.get_bases_pt tex
  | _ -> []

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
