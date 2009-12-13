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

open Format
open Dvi_util

type color = 
  | RGB of float * float * float
  | CMYK of float * float * float * float
  | HSB of float * float * float
  | Gray of float

(* a state can be push pop *)
type state = {
  h : int32;
  v : int32;
  w : int32;
  x : int32;
  y : int32;
  z : int32;
}
    (* an env can't *)
type 'a env = {
  dev : 'a;
  mutable ecolor : color;
  color_stack : color Stack.t;
  conv : float;
  mutable font : Int32.t;
  stack : state Stack.t;
  mutable s : state
}

type info = {
  color : color
}

let dumb_info = {color = Gray 0.}

let info_of_env env = {color = env.ecolor}

let new_env dev conv = {dev = dev;
                        ecolor = Gray 0.;
                        conv = conv;
                        font = Int32.zero;
                        stack = Stack.create ();
                        s = {h=Int32.zero; v=Int32.zero; 
                             w=Int32.zero; x=Int32.zero; 
                             y=Int32.zero; z=Int32.zero; 
                            };
                        color_stack = Stack.create ();
                       }

let rec scanf_with s def = function
  | [] -> def s
  | a::l -> try a s
    with Scanf.Scan_failure _ | Failure _ | End_of_file 
      -> scanf_with s def l
  

module type dev =
     sig
       type t
       type arg
       type cooked
       val new_document : arg -> Dvi.t -> t
       val new_page : t -> unit
       val fill_rect : t -> info -> float -> float -> float -> float -> unit
         (* fill_rect t x y w h *)
       val draw_char : t -> info -> Fonts.t -> Int32.t -> float -> float -> unit
         (* draw_char t font code x y *)
       val specials : t -> info -> string -> float -> float -> unit
         (* specials t s x y *)
       val end_document : t -> cooked
     end


module Interp (Dev : dev) =
struct
  let verbose = ref false
  let set_verbosity b =
    verbose := b;
    Fonts.set_verbosity b

  let debug = ref false
  let set_debug = (:=) debug

  let reset dev conv = 
    Dev.new_page dev;
    new_env dev conv

  let print_state fmt s =
    fprintf fmt "{h = %ld; v = %ld; w = %ld; x = %ld; y = %ld; z= %ld}@."
      s.h s.v s.w s.x s.y s.z

  let put_char env font code =
      let x = env.conv *. (Int32.to_float env.s.h)
      and y = env.conv *. (Int32.to_float env.s.v) in
      Dev.draw_char env.dev (info_of_env env) font code x y

  let put_rule env a b =
    let x = env.conv *. (Int32.to_float env.s.h)
    and y = env.conv *. (Int32.to_float env.s.v)
    and w = env.conv *. (Int32.to_float b)
    and h = env.conv *. (Int32.to_float a) in
    Dev.fill_rect env.dev (info_of_env env) x (y -. h) w h

  let interp_command fm env = function  
    | Dvi.SetChar c -> 
        if !debug then printf "Setting character %ld.@." c;
        let font = Int32Map.find env.font fm in
        let fwidth = Fonts.char_width font (Int32.to_int c) in
        let width = Int32.of_float fwidth in
	if !debug then printf "Character found in font %ld. Width = %ld@." 
	  env.font width;
        put_char env font c;
	env.s <- {env.s with h = Int32.add env.s.h width}
    | Dvi.SetRule(a, b) ->
        if !debug then printf "Setting rule (w=%ld, h=%ld).@." a b;
        put_rule env a b;
        env.s <- {env.s with h = Int32.add env.s.h b}
    | Dvi.PutChar c -> 
        if !debug then printf "Putting character %ld.@." c;
        put_char env (Int32Map.find env.font fm) c
    | Dvi.PutRule(a, b) ->
        if !debug then printf "Putting rule (w=%ld, h=%ld).@." a b;
        put_rule env a b
    | Dvi.Push -> 
        if !debug then printf "Push current state.@.";
        Stack.push env.s env.stack
    | Dvi.Pop ->      
        (try 
	   if !debug then printf "Pop current state.@.";
	   env.s <- Stack.pop env.stack
         with Stack.Empty -> failwith "Empty stack !")
    | Dvi.Right b -> 
        if !debug then printf "Moving right %ld.@." b;
        env.s<-{env.s with h = Int32.add env.s.h b}
    | Dvi.Wdefault -> 
        if !debug then printf "Moving right by the default W.@.";
        env.s<-{env.s with h = Int32.add env.s.h env.s.w}
    | Dvi.W b -> 
        if !debug then printf "Moving right and changing W to %ld.@." b;
        env.s<-{env.s with h = Int32.add env.s.h b; w = b}
    | Dvi.Xdefault -> 
        if !debug then printf "Moving right by the default X.@.";
        env.s<-{env.s with h = Int32.add env.s.h env.s.x}
    | Dvi.X b -> 
        if !debug then printf "Moving right and changing X to %ld.@." b;
        env.s<-{env.s with h = Int32.add env.s.h b; x = b}
    | Dvi.Down a ->
        if !debug then printf "Moving down %ld.@." a;
        env.s <- {env.s with v = Int32.add env.s.v a}
    | Dvi.Ydefault ->
        if !debug then printf "Moving down by the default Y.@.";
        env.s <- {env.s with v = Int32.add env.s.v env.s.y}
    | Dvi.Y a -> 
        if !debug then printf "Moving down and changing Y to %ld.@." a;
        env.s <- {env.s with v = Int32.add env.s.v a; y = a}
    | Dvi.Zdefault ->
        if !debug then printf "Moving down by the default Z.@.";
        env.s <- {env.s with v = Int32.add env.s.v env.s.z}
    | Dvi.Z a ->
        if !debug then printf "Moving down and changing Z to %ld.@." a;
        env.s <- {env.s with v = Int32.add env.s.v a; z = a}
    | Dvi.FontNum f -> 
        env.font <- f;
        if !debug then printf "Font is now set to %ld@." f
    | Dvi.Special xxx ->
        if !debug then printf "Special command : %s@." xxx;
        let x = env.conv *. (Int32.to_float env.s.h)
        and y = env.conv *. (Int32.to_float env.s.v) in
        let push color = 
          Stack.push env.ecolor env.color_stack; 
          env.ecolor <- color in
        scanf_with
          xxx
          (fun s -> Dev.specials env.dev (info_of_env env) s x y)
          [(fun s -> Scanf.sscanf s "color push rgb %f %f %f" 
              (fun r g b -> push (RGB (r,g,b))));
           (fun s -> Scanf.sscanf s "color push cmyk %f %f %f %f" 
              (fun c m y k -> push (CMYK(c,m,y,k))));
           (fun s -> Scanf.sscanf s "color push gray %f" 
              (fun g -> push (Gray(g))));
           (fun s -> Scanf.sscanf s "color push hsb %f %f %f" 
              (fun h s b -> push (HSB(h,s,b))));
           (fun s -> Scanf.sscanf s "color pop%n" 
              (fun _ -> env.ecolor <- Stack.pop env.color_stack));]
	  
  let interp_page dev conv fm p =
    List.iter (interp_command fm (reset dev conv))
      (List.rev (Dvi.commands p))
      
  let load_fonts font_map conv =
    Int32Map.fold (fun k fdef -> 
		     Int32Map.add k (Fonts.load_font fdef conv)
                  )
      font_map Int32Map.empty

  let load_doc arg doc =
    let conv = Dvi.get_conv doc in
    let fonts = load_fonts (Dvi.fontmap doc) conv in
    let dev = Dev.new_document arg doc in
    List.iter (fun p -> 
                 if !debug then
		   printf "#### Starting New Page ####@."
                 else if !verbose then printf ".";
		interp_page dev conv fonts p)
      (Dvi.pages doc);
    Dev.end_document dev


  let load_file arg file =
    let doc = Dvi.read_file file in
    if !verbose then
      printf "Dvi file parsing and interpretation :@.@?";
    let res = load_doc arg doc in
    if !verbose then
      printf " done@.@?";
    res

end
