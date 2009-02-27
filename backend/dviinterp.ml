open Format
open Dvi
open Fonts
open Tfm

type state = {
  h : int32;
  v : int32;
  w : int32;
  x : int32;
  y : int32;
  z : int32;
}

module Interp
  (Dev : 
     sig
       type t
       type cooked
       val new_document : Dvi.t -> t
       val new_page : t -> unit
       val fill_rect : t -> float -> float -> float -> float -> unit
       val draw_char : t -> Fonts.t -> Int32.t -> float -> float -> unit
       val end_document : t -> cooked
     end) =
struct
  let debug = ref true
  let unsome = function None -> assert false | Some x -> x
  let current_font = ref Int32.zero
  let stack : state Stack.t = Stack.create ()
  let dev = ref None
  let conv = ref 1.
  let set_debug = (:=) debug

  let reset () = 
    Dev.new_page (unsome !dev);
    current_font := Int32.zero;
    Stack.clear stack;
    {h=Int32.zero; v=Int32.zero; 
     w=Int32.zero; x=Int32.zero; 
     y=Int32.zero; z=Int32.zero; 
    }

  let print_state fmt s =
    fprintf fmt "{h = %ld; v = %ld; w = %ld; x = %ld; y = %ld; z= %ld}@."
      s.h s.v s.w s.x s.y s.z

  let put_char s font code =
      let x = !conv *. (Int32.to_float s.h)
      and y = !conv *. (Int32.to_float s.v) in
      Dev.draw_char (unsome !dev) font code x y

  let put_rule s a b =
    let x = !conv *. (Int32.to_float s.h)
    and y = !conv *. (Int32.to_float s.v)
    and w = !conv *. (Int32.to_float b)
    and h = !conv *. (Int32.to_float a) in
    Dev.fill_rect (unsome !dev) x (y -. h) w h

  let interp_command fm s = function  
    | SetChar c -> 
        if !debug then printf "Setting character %ld.@." c;
        let font = Int32Map.find !current_font fm in
        let idx = (Int32.to_int c) - font.metric.file_hdr.bc in
        let body = font.metric.body in
        let info = body.char_info.(idx) in
        let fwidth = body.width.(info.width_index) *. font.ratio in
        let width = Int32.of_float fwidth in
	if !debug then printf "Character found in font %ld. Width = %ld@." 
	  !current_font width;
        put_char s font c;
	{s with h = Int32.add s.h width}
    | SetRule(a, b) ->
        if !debug then printf "Setting rule (w=%ld, h=%ld).@." a b;
        put_rule s a b;
        {s with h = Int32.add s.h b}
    | PutChar c -> 
        if !debug then printf "Putting character %ld.@." c;
        put_char s (Int32Map.find !current_font fm) c;
        s
    | PutRule(a, b) ->
        if !debug then printf "Putting rule (w=%ld, h=%ld).@." a b;
        put_rule s a b;
        s
    | Push -> 
        if !debug then printf "Push current state.@.";
        Stack.push s stack; s
    | Pop ->      
        (try 
	   if !debug then printf "Pop current state.@.";
	   Stack.pop stack
         with Stack.Empty -> failwith "Empty stack !")
    | Right b -> 
        if !debug then printf "Moving right %ld.@." b;
        {s with h = Int32.add s.h b}
    | Wdefault -> 
        if !debug then printf "Moving right by the default W.@.";
        {s with h = Int32.add s.h s.w}
    | W b -> 
        if !debug then printf "Moving right and changing W to %ld.@." b;
        {s with h = Int32.add s.h b; w = b}
    | Xdefault -> 
        if !debug then printf "Moving right by the default X.@.";
        {s with h = Int32.add s.h s.x}
    | X b -> 
        if !debug then printf "Moving right and changing X to %ld.@." b;
        {s with h = Int32.add s.h b; x = b}
    | Down a ->
        if !debug then printf "Moving down %ld.@." a;
        {s with v = Int32.add s.v a}
    | Ydefault ->
        if !debug then printf "Moving down by the default Y.@.";
        {s with v = Int32.add s.v s.y}
    | Y a -> 
        if !debug then printf "Moving down and changing Y to %ld.@." a;
        {s with v = Int32.add s.v a; y = a}
    | Zdefault ->
        if !debug then printf "Moving down by the default Z.@.";
        {s with v = Int32.add s.v s.z}
    | Z a ->
        if !debug then printf "Moving down and changing Z to %ld.@." a;
        {s with v = Int32.add s.v a; z = a}
    | FontNum f -> 
        current_font := f;
        if !debug then printf "Font is now set to %ld@." f;
        s
    | Special xxx ->
        if !debug then printf "Special command ignored : %s@." xxx;
        s
	  
  let interp_page fm {commands = cmds} =
    ignore (List.fold_left (interp_command fm) (reset ()) (List.rev cmds))
      
  (* type font_def = { *)
  (*   checksum : int32; *)
  (*   scale_factor : int32; *)
  (*   design_size : int32; *)
  (*   area : string; *)
  (*   name : string; *)
  (* } *)


  let load_fonts mag font_map =
    Int32Map.fold (fun k fdef -> 
		     Int32Map.add k (Fonts.load_font mag (!conv) fdef)
                  )
      font_map Int32Map.empty

  let load_doc doc =
    let formule_magique_cm mag num den =
      ((Int32.to_float mag) *. ((Int32.to_float num) /. (Int32.to_float den))) /. (10.**8.) in
    conv := formule_magique_cm doc.preamble.pre_mag 
      doc.preamble.pre_num doc.preamble.pre_den;
    let fonts = load_fonts doc.preamble.pre_mag doc.font_map in
    dev := Some (Dev.new_document doc);
    List.iter (fun p -> 
		printf "#### Starting New Page ####@.";
		interp_page fonts p)
      doc.pages;
    Dev.end_document (unsome !dev)


  let load_file file =
    let doc = Dvi.read_file file in
    printf "Dvi file parsed successfully.@.";
    load_doc doc
end
