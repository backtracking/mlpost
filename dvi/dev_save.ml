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

open Tfm
open Fonts

type command = | Rectangle of Dviinterp.info*float * float * float * float (* x,y,w,h *)
               | Glyph of Dviinterp.info*Fonts.t * Int32.t * float * float
               | Specials of Dviinterp.info*string * float *float (* s,x,y *)

type page = { c : command list;
              x_min : float;
              y_min : float;
              x_max : float;
              y_max : float;
              bases : float list
            }
              
let stroke = 0.05
  

type t = {mutable pages : page list;
          doc : Dvi.t}

let replay_page_aux trace fill_rect draw_char specials dev page =
  List.iter (function |Rectangle (info,x,y,w,h) -> fill_rect dev info x y w h
               |Glyph (info,font,char,x,y) -> draw_char dev info font char x y
               |Specials (info,xxx,x,y) -> specials dev info xxx x y) page.c;
  if trace then 
    begin
      let h = page.y_max -. page.y_min in
      let w = page.x_max -. page.x_min in
      let msd x = x -. stroke/.2. in
      fill_rect dev Dviinterp.dumb_info page.x_min (msd page.y_min) w stroke;
      fill_rect dev Dviinterp.dumb_info (msd page.x_min) page.y_min stroke h;
      fill_rect dev Dviinterp.dumb_info page.x_min (msd page.y_max) w stroke;
      fill_rect dev Dviinterp.dumb_info (msd page.x_max) page.y_min stroke h
    end

let replay trace new_document new_page fill_rect draw_char specials end_document saved arg =
  let dev = new_document arg saved.doc in
  List.iter (fun page -> new_page dev; replay_page_aux trace fill_rect draw_char specials dev page) saved.pages;
  end_document dev

let separe_pages saved =
  List.map (fun page -> {pages = [page];doc=saved.doc}) saved.pages

let get_doc s = s.doc

let get_dimen_page s = 
  (s.x_min,s.y_min,s.x_max,s.y_max)

let get_dimen_first_page s = get_dimen_page (List.hd s.pages)

let get_bases_first_page s = (List.hd s.pages).bases

let nb_pages s = List.length s.pages

module Dev_save : Dviinterp.dev with type arg = bool
 with type cooked = t =
struct
  type arg = bool
  type cooked = t
  type t = { mutable tpages : page list;
             tdoc : Dvi.t;
             mutable tfirst_page : bool;
             mutable tc : command list;
             mutable tx_min : float;
             mutable ty_min : float;
             mutable tx_max : float;
             mutable ty_max : float;
             mutable tbases : float list;
             use_last_vrule : bool}



  let new_document use_last_vrule doc =
    {
      tpages = [];
      tdoc = doc;
      tfirst_page = true;
      tc = [];
      tx_min = infinity;
      tx_max = neg_infinity;
      ty_min = infinity;
      ty_max = neg_infinity;
      tbases = [];
      use_last_vrule = use_last_vrule
    }
  
  let new_page s =
    if s.tfirst_page 
    then s.tfirst_page<-false
    else
      begin
        let page = match s.use_last_vrule, s.tc with
          | false, _ -> {c = List.rev s.tc;
                         x_min = s.tx_min;
                         y_min = s.ty_min;
                         x_max = s.tx_max;
                         y_max = s.ty_max;
                         bases = s.tbases;
                        }
          | true, (Rectangle (_,x,y,_,h))::l -> {c = List.rev l;
                                               x_min = 0.;
                                               y_min = y;
                                               x_max = x;
                                               y_max = y+.h;
                                               bases = s.tbases;
                                              }
          | _ -> failwith "I thought there were always a vrule at the end, please report. thx"
        in
        s.tpages <- page::s.tpages;
        s.tc <- [];
        s.tx_min <- infinity;
        s.tx_max <- neg_infinity;
        s.ty_min <- infinity;
        s.ty_max <- neg_infinity;
        s.tbases <- []
      end

  let fill_rect s info x y w h =
    s.tc <- (Rectangle (info,x,y,w,h))::s.tc;
    if s.use_last_vrule then
      let xmin,xmax = x,x+.w(*min x (x+.w), max x (x+.w)*) in
      let ymin,ymax = y,y+.h(*min y (y+.h), max y (y+.h)*) in
      s.tx_min <- (min s.tx_min xmin);
      s.ty_min <- (min s.ty_min ymin);
      s.tx_max <- (max s.tx_max xmax);
      s.ty_max <- (max s.ty_max ymax)



  let draw_char s info font char x y =
    s.tc <- (Glyph (info,font,char,x,y))::s.tc;
    if not (List.mem y s.tbases) then s.tbases <- y::s.tbases;
    if s.use_last_vrule then
      let width,height,depth = Fonts.char_dims font (Int32.to_int char) in
      s.tx_min <- min s.tx_min x;
      s.ty_min <- min s.ty_min (y-.depth);
      s.tx_max <- max s.tx_max (x+.width);
      s.ty_max <- max s.ty_max (y+.height)

  let specials s info xxx x y =
    s.tc <- (Specials (info,xxx,x,y))::s.tc

  let end_document s =
    new_page s;
    {pages = List.rev s.tpages; doc = s.tdoc}
end
  
module Dev_load (Dev : Dviinterp.dev) =
struct
  let replay trace = replay trace Dev.new_document Dev.new_page Dev.fill_rect Dev.draw_char Dev.specials Dev.end_document
    
  let load_doc saved doc arg = 
    if get_doc saved = doc then replay false saved arg
    else invalid_arg ("The dvi doc is different")
end
