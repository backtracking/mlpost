open Format
open Dvi

type state = {
  h : int32;
  v : int32;
  w : int32;
  x : int32;
  y : int32;
  z : int32;
}

let debug = ref true

let current_font = ref Int32.zero
let stack : state Stack.t = Stack.create ()

let reset () = 
  current_font := Int32.zero;
  Stack.clear stack;
  {h=Int32.zero; v=Int32.zero; 
   w=Int32.zero; x=Int32.zero; 
   y=Int32.zero; z=Int32.zero; 
  }

let print_state fmt s =
  fprintf fmt "{h = %ld; v = %ld; w = %ld; x = %ld; y = %ld; z= %ld}@."
    s.h s.v s.w s.x s.y s.z

let interp_command fm s = function  
  | SetChar c -> 
      if !debug then printf "Setting character %ld.@." c;
      let tmf = Int32Map.find !current_font fm in
      let idx = (Int32.to_int c) - tmf.Tfm.file_hdr.Tfm.bc in
      let info = tmf.Tfm.body.Tfm.char_info.(idx) in
      let width = tmf.Tfm.body.Tfm.width.(info.Tfm.width_index) in
	if !debug then printf "Character found in font %ld. Width = %f@." 
	  !current_font width;
	{s with h = Int32.add s.h (Int32.of_float width)}
  | SetRule(a, b) ->
      if !debug then printf "Setting rule (w=%ld, h=%ld).@." a b;
      {s with h = Int32.add s.h b}
  | PutChar c -> 
      if !debug then printf "Putting character %ld.@." c;
      s
  | PutRule(a, b) ->
      if !debug then printf "Putting rule (w=%ld, h=%ld).@." a b;
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
  List.fold_left (interp_command fm) (reset ()) (List.rev cmds)
  
(* type font_def = { *)
(*   checksum : int32; *)
(*   scale_factor : int32; *)
(*   design_size : int32; *)
(*   area : string; *)
(*   name : string; *)
(* } *)

let kwhich = "kpsewhich"

let load_font fd =
  if !debug then
    printf "Loading font %s at [%ld/%ld]...@." 
      fd.name fd.Dvi.scale_factor fd.Dvi.design_size;
  let filename =
    if fd.area <> "" then 
      Filename.concat fd.area fd.name
    else
      let temp_fn = Filename.temp_file "font_path" "" in
      let exit_status =
	Sys.command
	  (Printf.sprintf "%s %s.tfm > %s" kwhich fd.name temp_fn) in
	if exit_status <> 0 then dvi_error "kwhich failed"
	else
	  let cin = open_in temp_fn in
	  let n =
	    try input_line cin
	    with _ ->
	      close_in cin; Sys.remove temp_fn; dvi_error "Cannot find font"
	  in
	    close_in cin; Sys.remove temp_fn; n
  in
    if !debug then
      printf "Trying to find metrics at %s...@." filename;
    let tfm = Tfm.read_file filename in
      if (Int32.compare tfm.Tfm.body.Tfm.header.Tfm.checksum 
	    fd.Dvi.checksum <> 0) then
	dvi_error "Metrics checksum do not match !.@.";
      if !debug then
	printf "Metrics successfully loaded for font %s from %s.@." 
	  fd.name filename;
      tfm

let load_fonts font_map =
  Int32Map.fold (fun k fdef -> Int32Map.add k (load_font fdef)) 
    font_map Int32Map.empty

let load_file file =
  let doc = Dvi.read_file file in
    printf "Dvi file parsed successfully.@.";
    let fonts = load_fonts doc.font_map in
      List.iter (fun p -> 
		   printf "#### Starting New Page ####@.";
		   ignore (interp_page fonts p)) 
	doc.pages

let _ =
  match Array.length Sys.argv with
    | 1 -> 
	printf "Usage : dviinterp <file1.dvi> <file2.dvi> ...\n"
    | n ->
	for i = 1 to n-1 do
	  let s = Sys.argv.(i) in load_file s
	done
